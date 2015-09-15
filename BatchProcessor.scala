// BatchProcessor.scala
package estimator
import org.apache.commons.math3.ode._
import org.apache.commons.math3.ode.nonstiff.AdamsBashforthIntegrator
import math.abs
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D

class BatchProcessor(
	motionEquations: (Double, (Vector3D, Vector3D)) => (Vector3D, Vector3D),
	observationEquations: Vector[Double] => Vector[Double],
	minStepSize:Double,
	absErrorTol:Vector[Double],
	relErrorTol:Vector[Double]
	) {
	// Equations of motion in 1st order form, dy = f(t, y)

	def run(initialState:(Double, (Vector3D, Vector3D)), intialUncertainty:(Vector3D, Vector3D), t:Double) = {
		val integrator = new AdamsBashforthIntegrator(4, minStepSize, 0.1*abs(t - initialState(0)), absErrorTol, relErrorTol)
		val equations = new MotionEquationsWrapper(motionEquations, motionEquations(initialState).Length)
		val state = new ExpandableStatefulODE(equations)
		state.setTime(initialState(0))
		state.setPrimaryState(initialState(1))

		integrator.integrate(state, t)
		state.getPrimaryState.toVector
	}
}

class MotionEquationsWrapper(
	motionEquations: (Double, (Vector3D, Vector3D)) => Vector[Double],
	dimension:Int
	) extends FirstOrderDifferentialEquations {

	def getDimension = dimension

	def computeDerivatives(t:Double, y:(Vector3D, Vector3D), yDot:(Vector3D, Vector3D)):Unit = {
		yDot = motionEquations(t, y)
	}
}
