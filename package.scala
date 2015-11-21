// package.scala
package com.jaketimothy

package object estimator {
	type FirstOrderDifferentialEquations = org.apache.commons.math3.ode.FirstOrderDifferentialEquations

	type DenseVector = breeze.linalg.DenseVector
	type DenseMatrix = breeze.linalg.DenseMatrix
}