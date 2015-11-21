// package.scala
package com.jaketimothy

package object estimator {
	type FirstOrderDifferentialEquations = org.apache.commons.math3.ode.FirstOrderDifferentialEquations

	type DenseVector[T] = breeze.linalg.DenseVector[T]
	val DenseVector = breeze.linalg.DenseVector
	type DenseMatrix[T] = breeze.linalg.DenseMatrix[T]
	val DenseMatrix = breeze.linalg.DenseMatrix
}