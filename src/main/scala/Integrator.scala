// Integrator.scala
package com.jaketimothy.estimator

import org.apache.commons.math3.ode.ExpandableStatefulODE
import org.apache.commons.math3.ode.nonstiff.AdamsBashforthIntegrator

object Integrator {

	def step(
		state: DenseVector[Double],
		motionEquations: FirstOrderDifferentialEquations,
		dt: Double,
		minStepSize: Double = 0.0,
		absErrorTol: Double = 1e-6,
		relErrorTol: Double = 1e-3
		): DenseVector[Double] = {

		val integrator = new AdamsBashforthIntegrator(
			4,
			minStepSize,
			0.1 * scala.math.abs(dt),
			absErrorTol,
			relErrorTol)
		val ode = new ExpandableStatefulODE(motionEquations)
		ode.setTime(0.0)
		ode.setPrimaryState(state.toArray)
		integrator.integrate(ode, dt)
		DenseVector(ode.getPrimaryState)
	}
}
