// Station.scala
package estimator

import breeze.linalg._
import math.{atan2, atan, Pi}

case class StationInfo(
	location: Array[Double],
	locationUncertaintyCovariance: Array[Double],
	bias: Array[Double],
	biasUncertaintyCovariance: Array[Double]
	)

trait Station {
	// ECEF coordinates

	def location: DenseVector[Double]
	def locationUncertaintyCovariance: DenseMatrix[Double]
	def bias: DenseVector[Double]
	def biasUncertaintyCovariance: DenseMatrix[Double]

	def observationUncertaintyCovariance: DenseMatrix[Double]

	def observationFromState(state: DenseVector[Double], t: Double): DenseVector[Double]
}

object Station {

	// Station generator
	def apply(info: StationInfo) = info.bias.length match {
		case 1 => RangeStation(
			DenseVector(info.location),
			new DenseMatrix(3, 3, info.locationUncertaintyCovariance),
			DenseVector(info.bias),
			new DenseMatrix(1, 1, info.biasUncertaintyCovariance))
		case 3 => RAEStation(
			DenseVector(info.location),
			new DenseMatrix(3, 3, info.locationUncertaintyCovariance),
			DenseVector(info.bias),
			new DenseMatrix(3, 3, info.biasUncertaintyCovariance))
	}
}

case class RangeStation(
	location: DenseVector[Double],
	locationUncertaintyCovariance: DenseMatrix[Double],
	bias: DenseVector[Double],
	biasUncertaintyCovariance: DenseMatrix[Double]
	) extends Station {

	assume(location.length == 3)
	assume(locationUncertaintyCovariance.rows == 3 && locationUncertaintyCovariance.cols == 3)
	assume(bias.length == 1)
	assume(biasUncertaintyCovariance.rows == 1 && biasUncertaintyCovariance.cols == 1)

	override val observationUncertaintyCovariance = {
		biasUncertaintyCovariance + estimator.linalg.norm(locationUncertaintyCovariance)
	}

	override def observationFromState(state: DenseVector[Double], t: Double) = {

		require(state.length == 3)

		DenseVector(norm(state(0 to 2) - location)) + bias
	}
}

case class RAEStation(
	location: DenseVector[Double],
	locationUncertaintyCovariance: DenseMatrix[Double],
	bias: DenseVector[Double],
	biasUncertaintyCovariance: DenseMatrix[Double]
	) extends Station {
	// only positive values for azimuth

	assume(location.length == 3)
	assume(locationUncertaintyCovariance.rows == 3 && locationUncertaintyCovariance.cols == 3)
	assume(bias.length == 3)
	assume(biasUncertaintyCovariance.rows == 3 && biasUncertaintyCovariance.cols == 3)

	override val observationUncertaintyCovariance = {
		val alpha = 1.0
		val weights = UnscentedTransformation.weights(3, alpha)
		val scale = UnscentedTransformation.scaleFactor(alpha)
		val chi = UnscentedTransformation.sigmaPoints(
			new Estimate(location, locationUncertaintyCovariance),
			scale)
		val gamma = chi.map(observationFromState(_, 0.0))
		val y = (weights, gamma).zipped.map((w, g) => w._1 * g).reduce(_ + _)
		biasUncertaintyCovariance + (weights, gamma).zipped.map(
					(w, g) => {
						val dY = g - y
						w._2 * dY * dY.t
						}).reduce(_ + _)
	}

	override def observationFromState(state: DenseVector[Double], t: Double) = {

		require(state.length == 3)

		val r = state(0 to 2) - location
		val a = -atan2(r(1), r(0))
		val e = atan(r(2) / norm(r(0 to 1)))
		DenseVector(norm(r), if (a > 0.0) a else 2.0 * Pi + a, e) + bias
	}
}