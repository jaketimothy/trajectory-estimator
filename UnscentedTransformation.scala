// UnscentedTransformation.scala
package estimator

import breeze.linalg._
import math.sqrt
import org.apache.commons.math3.ode._

object UnscentedTransformation {

	def weights(
		degree: Int,
		alpha: Double
		): Vector[(Double, Double)] = {

		require(alpha >= 1.0e-4 && alpha <= 1.0)

		val lambda = 3.0 * alpha * alpha - degree
		val w0 = lambda / (degree + lambda)
		val wi = 1.0 / (2.0 * (degree + lambda))
		(w0, w0 + 3.0 - alpha * alpha) +: Vector.fill(2 * degree)((wi, wi))
	}

	def scaleFactor(alpha: Double) = sqrt(3.0) * alpha

	def sigmaPoints(
		stateEstimate: Estimate,
		scaleFactor: Double
		): Vector[DenseVector[Double]] = {

		val n = centerPoint.length
		val a = cholesky(stateEstimate.covariance)
		stateEstimate.state +:
			Vector.tabulate(n){j => stateEstimate.state + scaleFactor * a(::, j)} ++
			Vector.tabulate(n){j => stateEstimate.state - scaleFactor * a(::, j)}
	}

	def stateEstimate(
		sigmaPoints: Vector[DenseVector[Double]],
		weights: Vector[(Double, Double)]
		): Estimate = {

		val state = (weights, sigmaPoints).zipped.map((w, x) => w._1 * x).sum
		val covariance = (weights, sigmaPoints).zipped.map(
			(w, x) => {
				val dX = x - state
				w._2 * dX * dX.t
			}).sum
		new Estimate(state, covariance)
	}
}