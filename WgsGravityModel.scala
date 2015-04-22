// WgsGravityModel.scala
package estimator
import estimator.SphericalHarmonicFunctions
import math.{pow, abs}

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
			value += pow(Earth.radius / radius, n) * innervalue
		}
		Earth.geopotential / radius * value
	}
}

object Earth {
    val radius = 6378137.0 // m
    val flattening = 1.0 / 298.257223563
    val eccentricity = 0.0818191908426
    val angularvelocity = 7292115.0e-11
    val geopotential = 3.986004418e14 // m^3/s^2
    val g = 9.80665 // m/s^2
}
