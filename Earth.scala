// Earth.scala
package estimator.planetmodel

class Earth(
	val earthType: EarthModelType,
	harmonicCoefficients: Vector[Vector[(Double, Double)]] = Vector.empty
	) extends Planet(harmonicCoefficients) {

	override val referenceEllipsoid = earthEllipsoids(earthType)
}

object Earth {

	object EarthModelType extends Enumeration {
		type EarthModelType = Value
		val WGS84 = Value
	}

	val earthEllipsoids = Map(
		EarthModelType.WGS84 -> ReferenceEllipsoid(6378137.0, 1.0 / 298.257223563, 7292115.1467e-11, 3986004.418e8))
}