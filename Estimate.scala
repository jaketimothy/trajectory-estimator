// Estimate.scala
package com.jaketimothy.estimator

import breeze.linalg._

case class Estimate(state: DenseVector[Double], covariance: DenseMatrix[Double])
