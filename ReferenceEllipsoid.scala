// ReferenceEllipsoid.scala
package estimator.planetmodel

import math._
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D

class ReferenceEllipsoid(
	val semimajorAxis:Double, // meters
	val flattening:Double,
	val angularVelocity:Double, // radians/second
	val gravitationalParameter:Double // meters^3/seconds^2, exoatmospheric
	) {
	val eccentricity = sqrt((2.0 - flattening) * flattening)
	val linearEccentricity = semimajorAxis * eccentricity
	val axisRatio = sqrt(1.0 - (2.0 - flattening) * flattening) // b/a
	val semiminorAxis = semimajorAxis * axisRatio

	def radiusOfCurvature(latitude:Double) = 
		semimajorAxis / sqrt(1.0 - pow(eccentricity * sin(latitude), 2))

	def ellipsoidalToCartesian(state:(Double,Double,Double)) = {
		val (latitude, longitude, altitude) = vector
		val n = radiusOfCurvature(latitude)
		Vector3D(
			(n + altitude) * cos(latitude) * cos(longitude),
			(n + altitude) * cos(latitude) * sin(longitude),
			(axisRatio * axisRatio * n + altitude) * sin(longitude))
	}

	def cartesianToEllipsoidal(state:Vector3D) = {
		val p = sqrt(state.getX * state.getX + state.getY * state.getY)
		val latitude = atan(state.getZ / (axisRatio * axisRatio * p))
		var n, altitude, latitude0:Double
		do {
			latitude0 = latitude
			n = radiusOfCurvature(latitude0)
			altitude = p / cos(latitude0) - n
			latitude = atan(state.getZ / ((1.0 - eccentricity * eccentricity * n / (n + altitude)) * p))
		} while( abs(latitude - latitude0) > abs(latitude) * 1e-15 )
		(latitude, atan(state.getY / state.getX), altitude)
	}
}