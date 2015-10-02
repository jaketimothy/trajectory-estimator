// UnscentedTransformation.scala
package estimator

import breeze.linalg._
import collection.immutable.{Vector => ScalaVector}
import math.sqrt
import org.apache.commons.math3.ode._

object UnscentedTransformation {

	def weights(
		degree: Int,
		alpha: Double
		): ScalaVector[(Double, Double)] = {

		require(alpha >= 1.0e-4 && alpha <= 1.0)

		val lambda = 3.0 * alpha * alpha - degree
		val w0 = lambda / (degree + lambda)
		val wi = 1.0 / (2.0 * (degree + lambda))
		(w0, w0 + 3.0 - alpha * alpha) +: ScalaVector.fill(2 * degree)((wi, wi))
	}

	def scaleFactor(alpha: Double) = sqrt(3.0) * alpha

	def sigmaPoints(
		stateEstimate: Estimate,
		scaleFactor: Double
		): ScalaVector[DenseVector[Double]] = {

		val n = stateEstimate.state.length
		val a = cholesky(stateEstimate.covariance)
		val xi = ScalaVector.tabulate(n){j => stateEstimate.state + scaleFactor * a(::, j)} ++
			ScalaVector.tabulate(n){j => stateEstimate.state - scaleFactor * a(::, j)}
		stateEstimate.state +: xi
	}

	def stateEstimate(
		sigmaPoints: ScalaVector[DenseVector[Double]],
		weights: ScalaVector[(Double, Double)]
		): Estimate = {

		val state = (weights, sigmaPoints).zipped.map((w, x) => w._1 * x).reduce(_ + _)
		val covariance = (weights, sigmaPoints).zipped.map(
			(w, x) => {
				val dX = x - state
				w._2 * dX * dX.t
			}).reduce(_ + _)
		new Estimate(state, covariance)
	}
}