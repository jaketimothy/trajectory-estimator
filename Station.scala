// Station.scala
package estimator

import breeze.linalg._
import math.{atan2, Pi}

trait Station {
	// ECEF coordinates

	val location: DenseVector[Double]

	val locationUncertainty: DenseVector[Double]

	val bias: DenseVector[Double]

	val biasUncertainty: DenseVector[Double]

	def observation(state: DenseVector[Double], t: Double): DenseVector[Double]
}

class RangeStation extends Station {

	def observation(state: DenseVector[Double], t: Double) = 
		new DenseVector(norm(state(0 to 2) - location))
}

class RAEStation extends Station {
	// only positive values for azimuth

	def observation(state: DenseVector[Double], t: Double) = {
		val r = state(0 to 2) - location
		val a = -atan2(r(1), r(0))
		val e = atan2(r(2), r(0 to 1).norm)
		new DenseVector(r.norm, if (a > 0.0) a else 2.0 * Pi + a, e)
	}
}