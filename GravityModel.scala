// GravityModel.scala
package estimator.planetmodel

import math._
import org.apache.commons.math3.geometry.euclidean.threed._

trait GravityModel {
	// coordinates are ECEF
	def gravitationalAcceleration(position:Vector3D):Vector3D
}

class PointGravityModel(val gravitationalParameter:Double) extends GravityModel {
	def gravitationalAcceleration(position:Vector3D) = 
		position.scalarMultiply(-gravitationalParameter / position.getNorm / position.getNormSq)
}

class EllipsoidalGravityModel(val referenceEllipsoid:ReferenceEllipsoid) extends GravityModel {
	def gravitationalAcceleration(position:Vector3D) = {
		// implemented from WGS84 (Jan 2000), chapter 4

		val c = referenceEllipsoid.linearEccentricity
		val b = referenceEllipsoid.semiminorAxis
		val omega = referenceEllipsoid.angularVelocity
		val a = referenceEllipsoid.semimajorAxis

		val p = sqrt(position.getX * position.getX + position.getY * position.getY)
		val u = sqrt(0.5 * (position.getNorm - c * c) *
			(1.0 + sqrt(1.0 + 4.0 * c * c * position.getZ * position.getZ / pow(position.getNorm - c * c, 2))))
		val beta = atan(position.getZ / u * sqrt(u * u + c * c) / p)
		val w = sqrt((u * u + pow(c * sin(beta), 2)) / (u * u + c * c))
		val q = 0.5 * ((1.0 + 3.0 * u * u / (c * c)) * atan(c / u) - 3.0 * u / c)
		val q0 = 0.5 * ((1.0 + 3.0 * b * b / (c * c)) * atan(c / b) - 3.0 * b / c)
		val qPrime = 3.0 * (1.0 + u * u / (c * c)) * (1.0 - u / c * atan(c / u)) - 1.0

		val gammaU = (-(referenceEllipsoid.gravitationalParameter + 
			omega * omega * a * a * c * qPrime / q0 * (0.5 * pow(sin(beta), 2) - 1.0 / 6.0)) / (u * u + c * c) +
			omega * omega * u * pow(cos(beta), 2)) / w
		val gammaBeta = (a * a / sqrt(u * u + c * c) * q / q0 - sqrt(u * u + c * c)) *
			omega * omega * sin(beta) * cos(beta) / w
		val gammaEllipsoidal = Vector3D(gammaU, gammaBeta, 0.0)

		val r = u / (w * sqrt(u * u + c * c))
		val ellipsoidalToCartesian = Rotation(Array(
			Array(r * cos(beta) * position.getX / p, -sin(beta) * position.getX / p / w, -position.getY / p),
			Array(r * cos(beta) * position.getY / p, -sin(beta) * position.getY / p / w, position.getX / p),
			Array(sin(beta) / w, r * coa(beta), 0.0)),
			1e-10)

		ellipsoidalToCartesian.applyTo(gammaEllipsoidal)
	}

	class SphericalHarmonicGravityModel(
		val referenceEllipsoid:ReferenceEllipsoid,
		val harmonicCoefficients:Vector[Vector[Double]]
		) extends GravityModel {
		val sphericalHarmonicFunctions = SphericalHarmonicFunctions(harmonicCoefficients.length)

		def degree = sphericalHarmonicFunctions.degree

		def gravitationalAcceleration(position:Vector3D) = {
			// implemented from EGM2008 (Apr 2012), section 2.2

			val latitude = acos(position.getZ / position.getNorm) // spherical latitude
			val longitude = atan(position.getY / position.getX)

			position.scalarMultiply(-referenceEllipsoid.gravitationalParameter / position.getNormSq * 
				(2 to degree).foldLeft(0.0)((sum, n) => 
					sum + pow(Ellipsoid.semimajorAxis / radius, n) * (-n to n).foldLeft(0.0)((innersum, m) => 
						innersum + harmonicCoefficients(n)(abs(m)) * sphericalHarmonics.normalizedValue(m, latitude, longitude))))
		}
	}

	object SphericalHarmonicGravityModel {
		def parseWgs84CoefficientsFile(file:String, degree:Int) = {
			val egmfile = io.Source.fromFile(file)
			val C = Array.fill(degree, degree + 1)(0.0)
			try {
				var o = 0
				egmfile.getLines.toStream.takeWhile(_ => o < degree).foreach(line => {
					val lineparts = line.trim.split("""\s+""")
					val d = lineparts(0).toInt
					o = lineparts(1).toInt
					val cvalues = lineparts.slice(2,6).map(_.toDouble)
					C(d,o) = cvalues(0)
					})
			} finally {
				egmfile.close
			}
			C
		}
	}

	class PointMassGravityModel(
		val gravitationalParameter:Double,
		val normalizedPointMasses:Vector[(Vector3D,Double)] // (location, value) pairs
		) extends GravityModel {
		def gravitationalAcceleration(position:Vector3D) = {
			gravitationalParameter * normalizedPointMasses.foldLeft(Vector3D(0.0, 0.0, 0.0))((g, pointMass) => {
				val r = pointMass._1.subtract(position)
				g.subtract(pointMass._2 / r.getNormSq / r.getNorm, r)
				})
		}
	}
}