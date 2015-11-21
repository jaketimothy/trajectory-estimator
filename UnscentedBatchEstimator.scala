// UnscentedBatchEstimator.scala
package com.jaketimothy.estimator

import breeze.linalg._
import math.{abs, sqrt}
import org.apache.spark.rdd.RDD
import org.apache.commons.math3.ode.{ExpandableStatefulODE, FirstOrderDifferentialEquations}
import org.apache.commons.math3.ode.nonstiff.AdamsBashforthIntegrator

class UnscentedBatchEstimator(
	val dimension: Int,
	def motionEquations: (DenseVector[Double], Double) => DenseVector[Double],
	val stations: Map[String, Station],
	val alpha: Double,
	val integrationMinStepSize: Double,
	val integrationAbsErrorTol: Double,
	val integrationRelErrorTol: Double
	) {

	val n = dimension

	// weights
	val (wM, wC) = UnscentedTransformation.weights(n, alpha)
	val scaleFactor = sqrt(3.0) * alpha

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

			chi += UnscentedTransformation.sigmaPoints(estimate.last, scaleFactor)
			var preTime = time
			for (i <- 0 until m) {
				val (t, stationKey, observation) = observations(i)
				chi += chi.last.map(Integrator.step(_, motionEquations, t - preTime))
				gamma += chi.last.map(observationEquations(_, t))

				val iRange = i * n to (i + 1) * n - 1
				yBar(iRange) = (wM, gamma.last).zipped.map((w, y) => w * y).sum
				y(iRange) = observation
				r(iRange, ::) = stations(stationKey).observationUncertaintyCovariance

				pY(iRange, ::) = r(iRange, ::) + (wC, gamma.last).zipped.map(
					(w, y) => {
						val dY = y - yBar(iRange)
						w * dY * dY.t
						}).sum
				pXY(iRange, ::) = (wC, chi.last, gamma.last).zipped.map(
					(w, x, y) => {
						val dY = y - yBar(iRange)
						w * (x - chi.last.head) * dY.t
						}).sum

				preTime = t
			}

			val k = (pY.t \ pXY.t).t
			val dY = y - yBar
			estimate += new Estimate(
				estimate.last.state + k * dY,
				estimate.head.covariance - k * pY * k.t)
			rmsOld = rmsNew
			rmsNew = (dY.t * (r \ dY)).sum / (n * m)
		} while (abs(rmsNew - rmsOld) / rmsOld < 1.0e5)

		estimate.last
	}
}