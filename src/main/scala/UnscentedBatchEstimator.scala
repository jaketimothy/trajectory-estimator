// UnscentedBatchEstimator.scala
package com.jaketimothy.estimator

import breeze.linalg.{DenseVector, DenseMatrix, *}
import scala.math.{abs, sqrt}
// import org.apache.spark.rdd.RDD
import org.apache.commons.math3.ode.FirstOrderDifferentialEquations
import org.apache.commons.math3.ode.nonstiff.GraggBulirschStoerIntegrator

class UnscentedBatchEstimator(
	val dimension: Int,
	val motionEquations: FirstOrderDifferentialEquations,
	val stations: Map[String, Station],
	val alpha: Double
	) {

	val n = dimension

	// weights
	val (meanWeights, covarianceWeights) = UnscentedTransformation.weights(n, alpha)
	val scaleFactor = UnscentedTransformation.scaleFactor(alpha)

	def estimate(
		time: Double,
		initialEstimate: Estimate,
		observations: Vector[(Double, String, DenseVector[Double])] // (time, stationKey, state)
		): Estimate = {

		val m = observations.length

		val estimate = new collection.mutable.ArrayBuffer[Estimate]()
		estimate += initialEstimate
		var rmsOld, rmsNew = 0.0
		do {
			val chi = new collection.mutable.ArrayBuffer[Vector[DenseVector[Double]]]()
			val gamma = new collection.mutable.ArrayBuffer[Vector[DenseVector[Double]]]()
			val yBar = DenseVector.zeros[Double](n * m)
			val y = DenseVector.zeros[Double](n * m)
			val r = DenseMatrix.zeros[Double](n * m, n)
			val pY = DenseMatrix.zeros[Double](n * m, n)
			val pXY = DenseMatrix.zeros[Double](n * m, n)

			chi += UnscentedTransformation.sigmaPoints(estimate.last.state, estimate.last.covariance, scaleFactor)
			var preTime = time
			for (i <- 0 until m) {
				val (t, stationKey, observation) = observations(i)
				val integrator = new GraggBulirschStoerIntegrator(0.0, t - preTime, 1.49012e-8, 1.49012e-8)
				chi += chi.last.map( x => {
					val tempY = Array.fill(n)(0.0)
					integrator.integrate(motionEquations, preTime, x.toArray, t, tempY)
					DenseVector(tempY)
					})
				gamma += chi.last.map(stations(stationKey).observationFromState(_, t))

				val iRange = i * n to (i + 1) * n - 1
				yBar(iRange) := (for (j <- 0 until n) yield meanWeights(j) * gamma.last(j)).reduce(_ + _)
				y(iRange) := observation
				r(iRange, ::) := stations(stationKey).observationUncertaintyCovariance

				pY(iRange, ::) := r(iRange, ::) + (for (j <- 0 until n) yield {
					val dY = gamma.last(j) - yBar(iRange)
					covarianceWeights(j) * dY * dY.t
					}).reduce(_ + _)
				pXY(iRange, ::) := (for (j <- 0 until n) yield {
					val dY = gamma.last(j) - yBar(iRange)
					covarianceWeights(j) * (chi.last(j) - chi.last.head) * dY.t
					}).reduce(_ + _)

				preTime = t
			}

			val k = (pY.t \ pXY.t).t
			val dY = y - yBar
			estimate += new Estimate(
				estimate.last.state + k * dY,
				estimate.head.covariance - k * pY * k.t)
			rmsOld = rmsNew
			rmsNew = (dY.t * (r \ dY)) / (n * m)
		} while (abs(rmsNew - rmsOld) / rmsOld < 1.0e5)

		estimate.last
	}
}