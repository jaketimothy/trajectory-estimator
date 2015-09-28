// Estimate.scala
package estimator

import breeze.linalg._

class Estimate(
	val state: DenseVector[Double],
	val covariance: DenseMatrix[Double])