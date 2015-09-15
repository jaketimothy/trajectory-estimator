// UnscentedKalmanFilter.scala
package estimator

import math.{abs, sqrt}
//import org.apache.spark.mllib.linalg.{Vector, Vectors, Matrix, Matrices}
import org.apache.spark.rdd.RDD
import breeze.linalg._
import org.apache.commons.math3.ode.{ExpandableStatefulODE, FirstOrderDifferentialEquations}
import org.apache.commons.math3.ode.nonstiff.AdamsBashforthIntegrator

class UnscentedKalmanFilter(
	motionEquations: (DenseVector[Double], Double) => DenseVector[Double],
	observationEquations: (DenseVector[Double], Double) => DenseVector[Double],
	stateEstimate: DenseVector[Double],
	stateTime: Double,
	stateCovariance: DenseMatrix[Double],
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

	val q = DenseMatrix.zeros(n, n) // process noise matrix
	val r = DenseMatrix.zeros(n, n) // ??? noise matrix

	// current (initial) state estimate
	var xHat = stateEstimate
	var p = stateCovariance
	var time = stateTime

	def sigmaPoints(
		centerPoint: DenseVector[Double],
		covarianceMatrix: DenseMatrix[Double],
		scale: Double
		): Vector[DenseVector[Double]] = {

		val a = cholesky(covarianceMatrix)
		centerPoint +:
			Vector.tabulate(n){j => centerPoint + scale * a(::, j)} ++
			Vector.tabulate(n){j => centerPoint - scale * a(::, j)}
	}

	def update(observations: Vector[(Double, DenseVector[Double])]): Unit = {

		observations.foreach{case(t, observation) => {
			val chii = sigmaPoints(xHat, p, gamma)

			val chii1 = chii.map(x => {
				integrationState.setTime(time)
				integrationState.setPrimaryState(x.toArray)
				val integrator = new AdamsBashforthIntegrator(
					4,
					integrationMinStepSize,
					abs(t - time), // TODO : determine effective way of reducing user-input integration parameters
					integrationAbsErrorTol,
					integrationRelErrorTol)
				integrator.integrate(integrationState, t)
				new DenseVector(integrationState.getPrimaryState)
				})

			val xBar = chii1.zip(wM).foldLeft(DenseVector.zeros(n)){(sumX, xw) => sumX + xw._1 * xw._2}
			val pBar = q + chii1.zip(wC).foldLeft(DenseMatrix.zeros(n, n)){(sumP, xw) => {
				val dX = xw._1 - xBar
				sumP + xw._2 * dX * dX.t
				}}

			val chii1bar = sigmaPoints(xBar, pBar, gamma)
			val gamma = chii1bar.map(x => observationEquations(x, t))
			val yBar = gamma.zip(wM).foldLeft(DenseVector.zeros(n)){(sumY, yw) => sumY + yw._1 * yw._2}
			
			val pyy = r + gamma.zip(wC).foldLeft(DenseMatrix.zeros(n, n)){(sumP, yw) => {
				val dY = yw._1 - yBar
				sumP + wx._2 * dY * dY.t
				}}
			val pxy = (0 to 2 * n).foldLeft(DenseMatrix.zeros(n, n)){(sumP, j) => {
				val dX = chii1(j) - xBar
				val dY = gamma(j) - yBar
				sumP + wC(j) * dX * dY.t
				}}

			val k = pxy * inv(pyy)
			xHat = xBar + k * (observation - yBar)
			p = pBar - k * pyy * k.t
			time = t
		}}
	}
}

class MotionEquationsWrapper(
	motionEquations: (DenseVector[Double], Double) => DenseVector[Double],
	dimension: Int
	) extends FirstOrderDifferentialEquations {

	def getDimension = dimension

	def computeDerivatives(t: Double, y: Array[Double], yDot: Array[Double]): Unit = {
		yDot = motionEquations(new DenseVector(y), t).toArray
	}
}

