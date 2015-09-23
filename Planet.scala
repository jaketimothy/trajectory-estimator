// Planet.scala
package estimator.planetmodel

abstract class Planet(harmonicCoefficients: Vector[Vector[(Double, Double)]] = Vector.empty) {

	def referenceEllipsoid: ReferenceEllipsoid

	val gravityModel: GravityModel = if (harmonicCoefficients.isEmpty)
		new EllipsoidalGravityModel(referenceEllipsoid)
	else
		new SphericalHarmonicGravityModel(referenceEllipsoid, harmonicCoefficients)
}