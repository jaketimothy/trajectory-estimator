// UnscentedTransformation.scala
package com.jaketimothy.estimator

import breeze.linalg.{DenseVector, DenseMatrix, *, cholesky}
import scala.math.sqrt

object UnscentedTransformation {

	// returns (meanWeights, covarianceWeights)
  def weights(
    degree: Int,
    alpha: Double,
    beta: Double = 2.0,
    kappa: Double = 0.0
    ) : (DenseVector[Double], DenseVector[Double]) = {

    require(alpha >= 1.0e-4 && alpha <= 1.0)

    val lambda = alpha * alpha * (degree + kappa) - degree
    val w0 = lambda / (degree + lambda)
    val wi = 1.0 / (2.0 * (degree + lambda))
    (DenseVector(w0 +: Array.fill(2 * degree)(wi)),
      DenseVector((w0 + (1.0 - alpha * alpha + beta)) +: Array.fill(2 * degree)(wi)))
  }

  def scaleFactor(alpha: Double) : Double = sqrt(3.0) * alpha

  def sigmaPoints(
  	means: DenseVector[Double],
  	covariance: DenseMatrix[Double],
    scaleFactor: Double
    ) : Vector[DenseVector[Double]] = {

    val a = cholesky(covariance)
    val xi = Vector.tabulate(means.length){j => means + scaleFactor * a(::, j)} ++
      Vector.tabulate(means.length){j => means - scaleFactor * a(::, j)}
    means +: xi
  }

  def transform(
    sigmaPoints: Vector[DenseVector[Double]],
    meanWeights: DenseVector[Double],
    covarianceWeights: DenseVector[Double],
    noiseCovariance: DenseMatrix[Double] = null
    ) : (DenseVector[Double], DenseMatrix[Double]) = {

  	val p = Option(noiseCovariance).getOrElse(DenseMatrix.zeros[Double](sigmaPoints(0).length, sigmaPoints(0).length))

    val state = (for (i <- 0 until sigmaPoints.length) yield meanWeights(i) * sigmaPoints(i)).reduce(_ + _)
    val covariance = (for (i <- 0 until sigmaPoints.length) yield {
      val dX = sigmaPoints(i) - state
      covarianceWeights(i) * dX * dX.t
      }).reduce(_ + _)

    (state, p + covariance)
  }
}
