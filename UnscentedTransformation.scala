// UnscentedTransformation.scala
package estimator

import breeze.linalg._

object UnscentedTransformation {

	def sigmaPoints(
		centerPoint: DenseVector[Double],
		covarianceMatrix: DenseMatrix[Double],
		scale: Double
		): Vector[DenseVector[Double]] = {

		val n = centerPoint.length
		val a = cholesky(covarianceMatrix)
		centerPoint +:
			Vector.tabulate(n){j => centerPoint + scale * a(::, j)} ++
			Vector.tabulate(n){j => centerPoint - scale * a(::, j)}
	}

}