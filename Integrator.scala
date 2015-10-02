// Integrator.scala
package estimator

import breeze.linalg.DenseVector
import org.apache.commons.math3.ode.{ExpandableStatefulODE, FirstOrderDifferentialEquations}
import org.apache.commons.math3.ode.nonstiff.AdamsBashforthIntegrator

object Integrator {

	def step(
		state: DenseVector[Double],
		motionEquations: (DenseVector[Double], Double) => DenseVector[Double],
		dt: Double,
		minStepSize: Double = 0.0,
		absErrorTol: Double = 1e-6,
		relErrorTol: Double = 1e-3
		): DenseVector[Double] = {

		val integrator = new AdamsBashforthIntegrator(
			4,
			minStepSize,
			0.1 * math.abs(dt),
			absErrorTol,
			relErrorTol)
		val ode = new ExpandableStatefulODE(new MotionEquationsWrapper(motionEquations, state.length))
		ode.setTime(0.0)
		ode.setPrimaryState(state.toArray)
		integrator.integrate(ode, dt)
		DenseVector(ode.getPrimaryState)
	}
}

class MotionEquationsWrapper(
	motionEquations: (DenseVector[Double], Double) => DenseVector[Double],
	dimension: Int
	) extends FirstOrderDifferentialEquations {

	override val getDimension = dimension

	override def computeDerivatives(t: Double, y: Array[Double], yDot: Array[Double]): Unit = {
		yDot = motionEquations(DenseVector(y), t).toArray
	}
}
