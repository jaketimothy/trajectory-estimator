// TestDataGenerator.scala
package estimator.test

import breeze.linalg.{DenseVector, norm}
import estimator.planetmodel.Earth
import estimator.planetmodel.Earth.EarthModelType

object TestDataGenerator extends App {
	
}

object CircularOrbitDataGenerator {

	def apply(
		initalPosition: DenseVector[Double],
		ascendingNodeAngle: Double,
		station: Station
		): Vector[DenseVector[Double]] = {

		val velocity = math.sqrt(Earth(WGS84).gravitationalParameter / norm(initalPosition))
	}
}