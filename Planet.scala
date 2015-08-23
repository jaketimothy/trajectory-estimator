// Planet.scala
package estimator.planetmodel

abstract class Planet(harmonicCoefficients:Vector[Vector[Double]] = Vector.empty) {
	val referenceEllipsoid:ReferenceEllipsoid
	val gravityModel:GravityModel = if (harmonicCoefficients.isEmpty)
			EllipsoidalGravityModel(referenceEllipsoid)
		else
			SphericalHarmonicGravityModel(referenceEllipsoid, harmonicCoefficients)
}