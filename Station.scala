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

	def observationUncertaintyCovariance: DenseMatrix[Double]

	def observationFromState(state: DenseVector[Double], t: Double): DenseVector[Double]
}

class RangeStation extends Station {

	override def observationUncertaintyCovariance = {
		norm(locationUncertaintyCovariance) + biasUncertaintyCovariance
	}

	override def observationFromState(state: DenseVector[Double], t: Double) = {
		(new DenseVector(norm(state(0 to 2) - location))) + bias
	}
}

class RAEStation extends Station {
	// only positive values for azimuth

	override def observationUncertaintyCovariance = {
		// TODO
	}

	override def observationFromState(state: DenseVector[Double], t: Double) = {
		val r = state(0 to 2) - location
		val a = -atan2(r(1), r(0))
		val e = atan2(r(2), r(0 to 1).norm)
		(new DenseVector(r.norm, if (a > 0.0) a else 2.0 * Pi + a, e)) + bias
	}
}