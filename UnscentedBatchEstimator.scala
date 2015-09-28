// UnscentedBatchEstimator.scala
package estimator

import math.{abs, sqrt}
import org.apache.spark.rdd.RDD
import breeze.linalg._
import org.apache.commons.math3.ode.{ExpandableStatefulODE, FirstOrderDifferentialEquations}
import org.apache.commons.math3.ode.nonstiff.AdamsBashforthIntegrator

class UnscentedBatchEstimator(
	motionEquations: (DenseVector[Double], Double) => DenseVector[Double],
	stations: Map[String, Station],
	val alpha: Double,
	val integrationMinStepSize: Double,
	val integrationAbsErrorTol: Array[Double],
	val integrationRelErrorTol: Array[Double]
	) {

	val n = stateEstimate.size

	// ivp integration state
	val integrationState = new ExpandableStatefulODE(new MotionEquationsWrapper(motionEquations, n))

	// weights
	val lambda = 3.0 * alpha * alpha - n
	val wi = Array.fill(2 * n){1.0 / (2.0 * (n + lambda))}
	val wM = new DenseVector(lambda / (n + lambda) +: wi)
	val wC = new DenseVector((wM(0) + 3.0 - alpha * alpha +: wi)
	val gamma = sqrt(n + lambda)

	def estimate(
		time: Double,
		initialEstimate: Estimate,
		observations: Vector[(Double, String, DenseVector[Double])] // (time, stationKey, state)
		): Estimate = {

		val m = observations.length

		var xHat = initialEstimate.state
		var pHat = initialEstimate.covariance
		var rmsOld, rmsNew = 0.0
		do {
			val chi = new collection.mutable.ArrayBuffer[Vector[DenseVector[Double]]]()
			val gamma = new collection.mutable.ArrayBuffer[Vector[DenseVector[Double]]]()
			val yBar = DenseVector.zeros[Double](n * m)
			val y = DenseVector.zeros[Double](n * m)
			val r = DenseMatrix.zeros[Double](n * m, n)
			val pY = DenseMatrix.zeros[Double](n * m, n)
			val pXY = DenseMatrix.zeros[Double](n * m, n)

			chi += UnscentedTransformation.sigmaPoints(xHat, pHat, gamma)
			var preTime = time
			for (i <- 0 until m) {
				val (t, station, observation) = observations(i)
				chi += chi.last.map(x => {
					integrationState.setTime(preTime)
					integrationState.setPrimaryState(x.toArray)
					val integrator = new AdamsBashforthIntegrator(
						4,
						integrationMinStepSize,
						abs(t - preTime), // TODO : determine effective way of reducing user-input integration parameters
						integrationAbsErrorTol,
						integrationRelErrorTol)
					integrator.integrate(integrationState, t)
					new DenseVector(integrationState.getPrimaryState)
					})
				gamma += chi.last.map(observationEquations(_, t))

				val iRange = i * n to (i + 1) * n - 1
				yBar(iRange) = gamma.last.zip(wM).foldLeft(DenseVector.zeros[Double](n))(
					(sumY, yw) => sumY + yw._1 * yw._2)
				y(iRange) = observation
				r(iRange, ::) = stations(station).observationUncertaintyCovariance

				pY(iRange, ::) = r(iRange, ::) + gamma.last.zip(wC).foldLeft(DenseMatrix.zeros[Double](n, n))(
					(sumP, yw) => {
						val dY = yw._1 - yBar
						sumP + wx._2 * dY * dY.t
						})
				pXY(iRange, ::) = (0 to 2 * n).foldLeft(DenseMatrix.zeros[Double](n, n))(
					(sumP, j) => {
						val dX = chi.last(j) - chi.last.head
						val dY = gamma.last(j) - yBar(iRange)
						sumP + wC(j) * dX * dY.t
						})

				preTime = t
			}

			val k = (pY.t \ pXY.t).t
			val dY = y - yBar
			xHat = xHat + k * dY
			rmsOld = rmsNew
			rmsNew = (dY.t * inv(r) * dY).sum / (n * m)
		} while (abs(rmsNew - rmsOld) / rmsOld < 1.0e5)
		new Estimate(xHat, )
	}
}