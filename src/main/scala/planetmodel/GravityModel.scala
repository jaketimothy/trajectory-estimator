// GravityModel.scala
package com.jaketimothy.estimator.planetmodel

import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import breeze.linalg.{DenseVector, DenseMatrix, norm, *}
import breeze.numerics._
import com.jaketimothy.estimator.math.DerivedLegendreFunctions

/*
 * Gravity models must calculate the gravitational acceleration at a given position
 * relative to the central body.  Coordinates are calculated in the ECEF frame.
 * 
 * References:
 *   [WGS84 2000] Department of Defense World Geodetic System 1984.
 *     http://earth-info.nga.mil/GandG/publications/tr8350.2/wgs84fin.pdf
 *   [Jones 2010] Efficient Models for the Evaluation and Estimation of the Gravity Field.
 *     http://ccar.colorado.edu/geryon/papers/Misc/bajones_phd.pdf
 * 
 * See also:
 *   http://www.mathworks.com/help/aerotbx/ug/gravitysphericalharmonic.html
 */
 
trait GravityModel {

	def gravitationalAcceleration(position: DenseVector[Double]): DenseVector[Double]
}

class PointGravityModel(val gravitationalParameter: Double) extends GravityModel {

	override def gravitationalAcceleration(position: DenseVector[Double]) = 
		-gravitationalParameter / pow(norm(position), 3) * position
}

/*
 * Implements the Ellipsoidal gravity model from [WGS84 2000] Chapter 4
 */
class EllipsoidalGravityModel(
	referenceEllipsoid: ReferenceEllipsoid,
	angularVelocity: Double,
	gravitationalParameter: Double
	) extends GravityModel {

	private val c = referenceEllipsoid.linearEccentricity
	private val b = referenceEllipsoid.semiminorAxis
	private val omega = angularVelocity
	private val a = referenceEllipsoid.semimajorAxis
	private val mu = gravitationalParameter

	override def gravitationalAcceleration(position: DenseVector[Double]) = {

		assume(norm(position) > a)

		val p = sqrt(position(0) * position(0) + position(1) * position(1))
		val u = sqrt(0.5 * (norm(position) * norm(position) - c * c) *
			(1.0 + sqrt(1.0 + 4.0 * c * c * position(2) * position(2) / pow(norm(position) * norm(position) - c * c, 2))))
		val beta = atan(position(2) / u * sqrt(u * u + c * c) / p)
		val w = sqrt((u * u + pow(c * sin(beta), 2)) / (u * u + c * c))
		val q = 0.5 * ((1.0 + 3.0 * u * u / (c * c)) * atan(c / u) - 3.0 * u / c)
		val q0 = 0.5 * ((1.0 + 3.0 * b * b / (c * c)) * atan(c / b) - 3.0 * b / c)
		val qPrime = 3.0 * (1.0 + u * u / (c * c)) * (1.0 - u / c * atan(c / u)) - 1.0
		
		val gammaU = (-(gravitationalParameter + omega * omega * a * a * c * qPrime / q0 * (0.5 * pow(sin(beta), 2) - 1.0 / 6.0)) 
			/ (u * u + c * c) + omega * omega * u * pow(cos(beta), 2)) / w
		val gammaBeta = (a * a / sqrt(u * u + c * c) * q / q0 - sqrt(u * u + c * c)) *
			omega * omega * sin(beta) * cos(beta) / w
		val gammaEllipsoidal = DenseVector(gammaU, gammaBeta, 0.0)

		val r = u / (w * sqrt(u * u + c * c))
		val ellipsoidalToCartesian = if (p == 0.0) {
			// if position is above z-pole, p == 0.0
			new DenseMatrix(3, 3, Array(
			0.0, 0.0, sin(beta) / w,
			0.0, 0.0, r * cos(beta),
			0.0, 0.0, 0.0))
		} else {
			new DenseMatrix(3, 3, Array(
			r * cos(beta) * position(0) / p, r * cos(beta) * position(1) / p, sin(beta) / w,
			-sin(beta) * position(0) / p / w, -sin(beta) * position(1) / p / w, r * cos(beta),
			-position(1) / p, position(0) / p, 0.0))
		}

		ellipsoidalToCartesian * gammaEllipsoidal
	}
}

/*
 * Implements the Cartesian (Gottlieb) model from [Jones 2010] Section 2.1.4
 */
class SphericalHarmonicGravityModel(
	val referenceEllipsoid: ReferenceEllipsoid,
	val harmonicCoefficients: RDD[(Int, Vector[(Double, Double)])]
	) extends GravityModel {

	def degree = harmonicCoefficients.count().toInt + 1

	private val sc = harmonicCoefficients.context

	// [Jones 2010] Equation 2.15
	private val getPi = Vector.tabulate(degree + 1, degree + 1)((n, m) => {
		sqrt((n + m + 1.0) * (n - m) * (if (m == 0) 0.5 else 1.0))
	})

	override def gravitationalAcceleration(position: DenseVector[Double]) = {
		
		val r = norm(position)
		val pq1 = (position(0) / r, position(1) / r)
		val pq = (1.0, 0.0) +: (1 to degree).scanLeft(pq1)((pqPrev, i) => 
			(pq1._1 * pqPrev._1 - pq1._2 * pqPrev._1, pq1._2 * pqPrev._2 + pq1._1 * pqPrev._2))
		
		//val d = harmonicCoefficients mapValues (cs => cs map {case (m, c, s) => (m, c * pq(m)._1 + s * pq(m)._2)}))
		//val e = harmonicCoefficients mapValues (cs => 0.0 +: cs map {case (m, c, s) => (m, c * pq(m)._1 + s * pq(m)._2)}))
		//val f = harmonicCoefficients mapValues (cs => cs map {case (m, c, s) => (m, c * pq(m)._1 + s * pq(m)._2)}))
		val d = harmonicCoefficients mapValues (cs => for (i <- 0 to cs.length) yield cs(i)._1 * pq(i)._1 + cs(i)._2 * pq(i)._2)
		val e = harmonicCoefficients mapValues (cs => 0.0 +: (for (i <- 1 to cs.length) yield cs(i)._1 * pq(i - 1)._1 + cs(i)._2 * pq(i - 1)._2))
		val f = harmonicCoefficients mapValues (cs => for (i <- 0 to cs.length) yield cs(i)._2 * pq(i)._1 - cs(i)._1 * pq(i)._2)

		def a(degree: Int, order: Int, x: Double) = DerivedLegendreFunctions.normalizedValue(degree, order, x)
		val rTerm = for (i <- 0 to degree) yield pow(referenceEllipsoid.semimajorAxis / r, i)
		val zOverR = position(2) / r
		//val a1 = for (i <- 2 to degree; j <- 1 to i) yield
		//val rTerm.drop(2).map{case (n, rho) => }
		val a1 = e.map{case (n, x) => rTerm(n) * (for (m <- 1 to n) yield m * a(n, m, zOverR) * x(m)).sum}.sum
		val a2 = f.map{case (n, x) => rTerm(n) * (for (m <- 1 to n) yield m * a(n, m, zOverR) * x(m)).sum}.sum
		val a3 = d.map{case (n, x) => rTerm(n) * (for (m <- 0 to n - 1) yield m * a(n, m, zOverR) * x(m) * getPi(n)(m + 1) / getPi(n)(m)).sum}.sum
		val a4 = d.map{case (n, x) => rTerm(n) * (for (m <- 1 to n) yield (n + m + 1.0) * a(n, m, zOverR) * x(m)).sum}.sum
		// val a1 = (2 to degree).foldLeft(0.0)((sum, n) =>
		// 	sum + rTerm(n) * (1 to n).foldLeft(0.0)((innerSum, m) =>
		// 		innerSum + m * a(n, m, zOverR) * e(n)(m)))
		// val a2 = (2 to degree).foldLeft(0.0)((sum, n) =>
		// 	sum + rTerm(n) * (1 to n).foldLeft(0.0)((innerSum, m) =>
		// 		innerSum + m * a(n, m, zOverR) * f(n)(m)))
		// val a3 = (2 to degree).foldLeft(0.0)((sum, n) =>
		// 	sum + rTerm(n) * (0 to n - 1).foldLeft(0.0)((innerSum, m) =>
		// 		innerSum + m * a(n, m + 1, zOverR) * d(n)(m) * getPi(n)(m + 1) / getPi(n)(m)))
		// val a4 = (2 to degree).foldLeft(0.0)((sum, n) =>
		// 	sum + rTerm(n) * (1 to n).foldLeft(0.0)((innerSum, m) =>
		// 		innerSum + (n + m + 1.0) * a(n, m, zOverR) * d(n)(m)))

		referenceEllipsoid.semimajorAxis / r / r * (-position / r + (
			(DenseVector(a1, a2, a3)) - (position(2) * a3 / r + a4) * position / r))
	}
}

object SphericalHarmonicGravityModel {

	def parseWgs84CoefficientsFile(
		sc: SparkContext,
		file: String,
		degree: Int
		): RDD[(Int, Vector[(Double, Double)])] = {

		// take: (3 to n + 1).sum == (n + 1) * (n + 2) / 2 - 3
		val parsedLines = sc.textFile(file).take((degree + 1) * (degree + 2) / 2 - 3)
			.map(line => {
				val lineparts = line.trim.split("""\s+""")
				val n = lineparts(0).toInt
				val m = lineparts(1).toInt
				val csvalues = lineparts.slice(2,6).map(_.toDouble)
				(n, (m, csvalues(0), csvalues(1)))
			  })

		sc.parallelize(
			parsedLines.groupBy(_._1)
			  .mapValues(x => x.map(_._2).sortBy(_._1).map(y => (y._2, y._3)).toVector)
			  .toSeq)
			  //.sortByKey()
	}
}

class PointMassGravityModel(
	val gravitationalParameter: Double,
	val normalizedPointMasses: Vector[(DenseVector[Double], Double)] // (location, value) pairs
	) extends GravityModel {

	override def gravitationalAcceleration(position: DenseVector[Double]) = {
		
		gravitationalParameter * normalizedPointMasses.foldLeft(DenseVector(0.0, 0.0, 0.0))((g, pointMass) => {
			val r = pointMass._1 - position
			val rMag = norm(r)
			g - pointMass._2 / rMag / rMag * r / rMag
			})
	}
}