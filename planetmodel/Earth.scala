// Earth.scala
package com.jaketimothy.estimator.planetmodel

abstract class WGS84Earth extends Planet {

    override val referenceEllipsoid = ReferenceEllipsoid(6378137.0, 1.0 / 298.257223563)
    override val angularVelocity = 7292115.1467e-11
    override val gravitationalParameter = 3986004.418e8
}

class PointWGS84Earth extends WGS84Earth {

    override val gravityModel = new PointGravityModel(gravitationalParameter)
}

class EllipsoidalWGS84Earth extends WGS84Earth {

    override val gravityModel = new EllipsoidalGravityModel(
        referenceEllipsoid, angularVelocity, gravitationalParameter)
}

class HarmonicWGS84Earth(
	harmonicCoefficients: Vector[Vector[(Double, Double)]]
	) extends WGS84Earth {

	override val gravityModel = new SphericalHarmonicGravityModel(
		referenceEllipsoid, harmonicCoefficients)
}