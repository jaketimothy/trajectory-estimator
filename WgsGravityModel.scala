// WgsGravityModel.scala
package estimator

import estimator.SphericalHarmonicFunctions
import math.{pow, abs, sqrt, cos, sin, atan, atan2}

class EllipsoidalGravityModel {
	private def matmul(a:Array[Array[Double]], b:Array[Array[Double]]) = {
		// matrix multiplication: a * b
		Array.tabulate(a.length, a.length)((n, m) => {
			a.view.zipWithIndex.reduceLeft(
				case (sum, (x, index)) => sum + x * b(index)(m)
				)
			})
	}

	private def force(p:(Double,Double,Double), latitude:Double, longitude:Double, altitude:Double) = {
		val a = Ellipsoid.semimajorAxis
		val b = Ellipsoid.semiminorAxis
		val c = Ellipsoid.linearEccentricity
		val omega = Ellipsoid.angularVelocity
		val pmag = p.reduceLeft((sum, v) => sum + v * v)
		val (x, y, z) = p
		val geocentricLatitude = atan((1 - pow(Ellipsoid.eccentricity, 2)) * tan(phi))

		val u = sqrt(0.5 * (pmag - c * c) * 
			(1.0 + sqrt(1.0 + 4.0 * c * c * z * z / pow(pmag - c * c, 2))))
		val beta = atan(z / u * sqrt((u * u + c * c) / (x * x + y * y)))
		val w = sqrt((u * u + pow(c * sin(beta), 2)) / (u * u + c * c))
		val q = 0.5 * ((1.0 + 3.0 * u * u / (c * c)) * atan(c / u) - 3.0 * u / c)
		val q0 = 0.5 * ((1.0 + 3.0 * b * b / (c * c)) * atan(c / b) - 3.0 * b / c)
		val qPrime = 3.0 * (1.0 + u * u / (c * c)) * (1.0 - u / c * atan(c / u)) - 1.0

		val gammaU = (
			-(
				Ellipsoid.geopotential + 
				omega * omega * a * a * c * (qPrime / q0) * (
					0.5 * pow(sin(beta), 2) - 1.0 / 6.0
					)
				) / (u * u + c * c) + omega * omega * u * pow(cos(beta), 2)
			) / w
		val gammaBeta = (
			a * a / sqrt(u * u + c * c) * q / q0 - sqrt(u * u + c * c)
			) * omega * omega * sin(beta) * cos(beta) / w
		val gammaEllipsoidal = Array(Array(gammaU), Array(gammaBeta), Array(0.0))

		val emag = u / (w * sqrt(u * u + c * c))
		val ellipsoidalToRectangular = Array(
			Array(
				emag * cos(beta) * cos(longitude),
				-sin(beta) * cos(longitude) / w,
				-sin(longitude)
				),
			Array(
				emag * cos(beta) * sin(longitude),
				-sin(beta) * sin(longitude) / w,
				cos(longitude)
				),
			Array(sin(beta) / w, emag * cos(beta), 0.0)
			)
		
		val gammaRectangular = matmul(ellipsoidalToRectangular, gammaEllipsoidal)
		(gammaRectangular(1)(1), gammaRectangular(2)(1), gammaRectangular(3)(1))
	}

	def forceFromRectangular(p:(Double,Double,Double)) = {

	}

	def forceFromGeodetic(latitude:Double, longitude:Double, altitude:Double) = {
		val n = Ellipsoid.semimajorAxis / sqrt(
			1.0 - pow(Ellipsoid.eccentricity * sin(latitude), 2))
		val p = (
			(n + altitude) * cos(latitude) * cos(longitude),
			(n + altitude) * cos(latitude) * sin(longitude),
			(Ellipsoid.axisRatio * Ellipsoid.axisRatio * n + altitude) * sin(latitude))
		force(p, latitude, longitude, altitude)
	}
}

class SphericalHarmonicGravityModel(degree:Int) {
	val EgmFilename = "EGM2008_to2190_TideFree"
	private val egmfile = io.Source.fromFile(EgmFilename)
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

	val sphericalHarmonics = SphericalHarmonicFunctions(degree)

	def T(radius:Double, theta:Double, lambda:Double) = {
		var value = 0.0
		for (n <- 2 to degree) {
			var innervalue = 0.0
			for (m <- -n to n) {
				innervalue += C(n,abs(m)) * sphericalHarmonics.normalizedValue(m, theta, lambda)
			}
			value += pow(Ellipsoid.semimajorAxis / radius, n) * innervalue
		}
		Ellipsoid.geopotential / radius * value
	}
}

object Ellipsoid {
	// defining values
    val semimajorAxis = 6378137.0 // m
    val flattening = 1.0 / 298.257223563
    val angularVelocity = 7292115.1467e-11 // rad/s
    val geopotential = 3986004.418e8 // m^3/s^2 (exoatmospheric)

    // derived values
    val eccentricity = sqrt((2.0 - flattening) * flattening)
    val linearEccentricity = semimajorAxis * eccentricity
    val axisRatio = sqrt(1.0 - (2.0 - flattening) * flattening) // b / a
    val semiminorAxis = semimajorAxis * axisRatio
}
