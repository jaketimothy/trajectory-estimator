// ReferenceEllipsoid.scala
package estimator.planetmodel

import math._
import breeze.linalg._

case class ReferenceEllipsoid(semimajorAxis: Double, flattening: Double) {
	// distances in meters, angles in radians

	val eccentricity = sqrt((2.0 - flattening) * flattening)
	val linearEccentricity = semimajorAxis * eccentricity
	val axisRatio = sqrt(1.0 - (2.0 - flattening) * flattening) // b/a
	val semiminorAxis = semimajorAxis * axisRatio

	def radiusOfCurvature(latitude: Double) = 
		semimajorAxis / sqrt(1.0 - pow(eccentricity * sin(latitude), 2))

	def ellipsoidalToCartesian(state: (Double,Double,Double)) = {
		val (latitude, longitude, altitude) = state
		val n = radiusOfCurvature(latitude)
		DenseVector(
			(n + altitude) * cos(latitude) * cos(longitude),
			(n + altitude) * cos(latitude) * sin(longitude),
			(axisRatio * axisRatio * n + altitude) * sin(longitude))
	}

	def cartesianToEllipsoidal(state: DenseVector[Double]) = {
		val p = norm(state(0 to 1))
		var latitude = atan(state(2) / (axisRatio * axisRatio * p))
		var n, altitude, latitude0 = 0.0
		do {
			latitude0 = latitude
			n = radiusOfCurvature(latitude0)
			altitude = p / cos(latitude0) - n
			latitude = atan(state(2) / ((1.0 - eccentricity * eccentricity * n / (n + altitude)) * p))
		} while (abs(latitude - latitude0) > abs(latitude) * 1e-15)
		(latitude, atan(state(1) / state(0)), altitude)
	}
}