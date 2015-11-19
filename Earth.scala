// Earth.scala
package estimator.planetmodel

object EarthModelType extends Enumeration {
	type EarthModelType = Value
	val WGS84 = Value
}
import EarthModelType._

class Earth(
	val earthType: EarthModelType,
	harmonicCoefficients: Vector[Vector[(Double, Double)]] = Vector.empty
	) extends Planet(harmonicCoefficients) {

	override val referenceEllipsoid = Earth.earthEllipsoids(earthType)
}

object Earth {

	val earthEllipsoids = Map(
		EarthModelType.WGS84 -> new ReferenceEllipsoid(6378137.0, 1.0 / 298.257223563, 7292115.1467e-11, 3986004.418e8))

	def apply(earthType: EarthModelType) = earthEllipsoids(earthType)
}