// TestDataGenerator.scala
package com.jaketimothy.estimator.test

import breeze.linalg.{DenseVector, DenseMatrix, norm}
import com.jaketimothy.estimator._
import com.jaketimothy.estimator.planetmodel._
import com.jaketimothy.estimator.math.cross
import math.{sin, cos, Pi}
import org.apache.commons.math3.ode.FirstOrderDifferentialEquations
import java.nio.file.{Files, Paths}
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.{writePretty}

object TestDataGenerator extends App {
	// potential test data:
	// http://spaceflight.nasa.gov/realdata/sightings/SSapplications/Post/JavaSSOP/orbit/ISS/SVPOST.html
	//

	val circleData = CircularOrbitDataGenerator(
		DenseVector(2231811.78, -3574677.39, 5301974.95),
		247.4627 / 180.0 * Pi,
		Station(
			Array(-2516715.36114, -4653003.08089, 3551245.35929),
			Array(1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0),
			Array(0.0),
			Array(4.0)),
		24*3600,
		60,
		100
		)

	val utf8 = java.nio.charset.StandardCharsets.UTF_8
	implicit val formats = Serialization.formats(NoTypeHints)
	Files.write(Paths.get("CircleData.json"), writePretty(circleData).getBytes(utf8))
}

object CircularOrbitDataGenerator {

	object MotionEquations extends FirstOrderDifferentialEquations {

		override val getDimension = 6

		val earth = new EllipsoidalWGS84Earth();

		override def computeDerivatives(t: Double, y: Array[Double], yDot: Array[Double]): Unit = {
			val derivatives = y.slice(3, 6) ++ earth.gravityModel.gravitationalAcceleration(DenseVector(y.slice(0, 3))).toArray
			derivatives.copyToArray(yDot)
		}
	}

	def apply(
		initalPosition: DenseVector[Double],
		ascendingNode: Double,
		station: Station,
		duration: Double,
		timeStep: Double,
		positionNoiseDeviation: Double
		): Vector[Vector[Double]] = {

		// velocity magnitude
		val v = math.sqrt(MotionEquations.earth.gravitationalParameter / norm(initalPosition))

		// ascending node direction
		val a = DenseVector(cos(ascendingNode), sin(ascendingNode), 0.0)

		// initial velocity computation
		val velocityDirection = cross(cross(a, initalPosition), initalPosition)
		val velocity = v * velocityDirection / norm(velocityDirection)

		val initialState = DenseVector(initalPosition.toArray ++ velocity.toArray)

		val stateData = initialState +: (timeStep to duration by timeStep).scanLeft(initialState)(
			(y, t) => Integrator.step(y, MotionEquations, timeStep))

		val noisyStateData = stateData.map(y => DenseVector(
			y(0 to 2).toArray.map(x => x + util.Random.nextGaussian() * positionNoiseDeviation) ++ y(3 to 5).toArray))

		noisyStateData.map(station.observationFromState(_, 0.0).toArray.toVector).toVector
	}
}