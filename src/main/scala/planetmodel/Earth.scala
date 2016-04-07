// Earth.scala
package com.jaketimothy.estimator.planetmodel

import org.apache.spark.rdd.RDD

/*
 * WGS84Earth takes its defining constants from [WGS84 2000].
 *
 * References:
 *   [WGS84 2000] Department of Defense World Geodetic System 1984.
 *     http://earth-info.nga.mil/GandG/publications/tr8350.2/wgs84fin.pdf
 */

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
	harmonicCoefficients: RDD[(Int, Vector[(Double, Double)])]
	) extends WGS84Earth {

	override val gravityModel = new SphericalHarmonicGravityModel(
		referenceEllipsoid, harmonicCoefficients)
}