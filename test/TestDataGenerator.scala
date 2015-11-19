// TestDataGenerator.scala
package estimator.test

import breeze.linalg.{DenseVector, DenseMatrix, norm}
import estimator.planetmodel.Earth
import estimator.planetmodel.Earth.EarthModelType
import estimator.linalg.Crossable
import math.{sin, cos, Pi}

object TestDataGenerator extends App {
	// potential test data:
	// http://spaceflight.nasa.gov/realdata/sightings/SSapplications/Post/JavaSSOP/orbit/ISS/SVPOST.html
	//
	
	val circleData = CircularOrbitDataGenerator(
		DenseVector(2231811.78, -3574677.39, 5301974.95),
		247.4627 / 180.0 * Pi,
		RangeStation(
			DenseVector(-2516715.36114, -4653003.08089, 3551245.35929),
			new DenseMatrix(3, 3, Array(1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0)),
			DenseVector(0.0),
			new DenseMatrix(1, 1, Array(4.0))),
		)
}

object CircularOrbitDataGenerator {

	def apply(
		initalPosition: DenseVector[Double],
		ascendingNode: Double,
		station: Station,
		duration: Double,
		timeStep: Double,
		positionNoise: Double
		): Vector[DenseVector[Double]] = {

		val earth = new Earth(WGS84);

		// velocity magnitude
		val v = math.sqrt(earth.gravitationalParameter / norm(initalPosition))

		// ascending node direction
		val a = DenseVector(cos(ascendingNode), sin(ascendingNode), 0.0)

		// initial velocity computation
		val velocityDirection = a cross initalPosition cross initalPosition
		val velocity = v * velocityDirection / norm(velocityDirection)

		val initialState = initalPosition ++ velocity

		// ivp ode
		def motionEquations(y: DenseVector[Double], t: Double): DenseVector[Double] = {
			// dy = f(y, t)

			y(3 to 5) ++ earth.gravityModel.gravitationalAcceleration(y(0 to 2))
		}

		val stateData = initialState +: (timeStep to duration by timeStep).scanLeft(initialState)(
			(x, t) => Integrator.step(x, motionEquations, timeStep))

		val noisyStateData = stateData.map(x => x(0 to 2).map(_ + util.Random.nextGaussian() * positionNoise) ++ x(3 to 5))

		noisyStateData.map(station.observationFromState(_, 0.0))
	}
}