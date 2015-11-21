// UnscentedTransformation.scala
package com.jaketimothy.estimator

import breeze.linalg.{*, cholesky}
import scala.math.sqrt

object UnscentedTransformation {

	def weights(
		degree: Int,
		alpha: Double
		): (Vector[Double], Vector[Double]) = {

		require(alpha >= 1.0e-4 && alpha <= 1.0)

		val lambda = 3.0 * alpha * alpha - degree
		val w0 = lambda / (degree + lambda)
		val wi = 1.0 / (2.0 * (degree + lambda))
		(w0 +: Vector.fill(2 * degree)(wi),
			(w0 + 3.0 - alpha * alpha) +: Vector.fill(2 * degree)(wi))
	}

	def scaleFactor(alpha: Double) = sqrt(3.0) * alpha

	def sigmaPoints(
		stateEstimate: Estimate,
		scaleFactor: Double
		): Vector[DenseVector[Double]] = {

		val n = stateEstimate.state.length
		val a = cholesky(stateEstimate.covariance)
		val xi = Vector.tabulate(n){j => stateEstimate.state + scaleFactor * a(::, j)} ++
			Vector.tabulate(n){j => stateEstimate.state - scaleFactor * a(::, j)}
		stateEstimate.state +: xi
	}

	def stateEstimate(
		sigmaPoints: Vector[DenseVector[Double]],
		weights: Vector[(Double, Double)]
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