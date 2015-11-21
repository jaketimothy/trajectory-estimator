// CrossProduct.scala
package estimator.math

import breeze.linalg.{DenseVector, DenseMatrix, *}

object cross {

    def apply(x: DenseVector[Double], y: DenseVector[Double]): DenseVector[Double] = {

        val skewSymmetricMatrix = new DenseMatrix(3, 3, Array[Double](
            0.0, x(2), -x(1), -x(2), 0.0, x(0), x(1), -x(0), 0.0))
        
        skewSymmetricMatrix * y
    }
}