// Station.scala
package estimator

import breeze.linalg._
import math.{atan2, Pi}

abstract class Station(
	val location: DenseVector[Double],
	val locationUncertaintyCovariance: DenseMatrix[Double],
	val bias: DenseVector[Double],
	val biasUncertaintyCovariance: DenseMatrix[Double]
	) {
	// ECEF coordinates

	assume(location.length == 3)
	assume(locationUncertaintyCovariance.rows == 3 && locationUncertaintyCovariance.cols == 3)

	def observationUncertaintyCovariance: DenseMatrix[Double]

	def observationFromState(state: DenseVector[Double], t: Double): DenseVector[Double]
}

class RangeStation extends Station {

	assume(bias.length == 1)
	assume(biasUncertaintyCovariance.rows == 1 && biasUncertaintyCovariance.cols == 1)

	override val observationUncertaintyCovariance = {
		norm(locationUncertaintyCovariance) + biasUncertaintyCovariance
	}

	override def observationFromState(state: DenseVector[Double], t: Double) = {

		require(state.length == 3)

		(new DenseVector(norm(state(0 to 2) - location))) + bias
	}
}

class RAEStation extends Station {
	// only positive values for azimuth

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
		val y = (weights, gamma).zipped.map((w, g) => w._1 * g).sum
		biasUncertaintyCovariance + (weights, gamma).zipped.map(
					(w, g) => {
						val dY = g - y
						w._2 * dY * dY.t
						}).sum
	}

	override def observationFromState(state: DenseVector[Double], t: Double) = {

		require(state.length == 3)

		val r = state(0 to 2) - location
		val a = -atan2(r(1), r(0))
		val e = atan(r(2) / r(0 to 1).norm)
		(new DenseVector(r.norm, if (a > 0.0) a else 2.0 * Pi + a, e)) + bias
	}
}