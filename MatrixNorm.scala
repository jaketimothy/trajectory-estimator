// MatrixNorm.scala
package estimator.linalg

import breeze.generic.UFunc
import breeze.linalg._
import breeze.linalg.support.CanTraverseValues
import breeze.linalg.support.CanTraverseValues.ValuesVisitor
import breeze.numerics.sqrt
import scala.language.higherKinds

object norm extends UFunc {
	// matrix Frobenius norm

	implicit def normFromTraversableDoubles[DenseMatrix[Double]](
		implicit traverse: CanTraverseValues[DenseMatrix[Double], Double]
		): Impl[DenseMatrix[Double], Double] = new Impl[DenseMatrix[Double], Double] {
		
		def apply(t: DenseMatrix[Double]): Double = {

			var sum = 0.0
			traverse.traverse(t, new ValuesVisitor[Double] {
				def visit(a: Double) = {sum += a * a}
				def zeros(count: Int, zeroValue: Double) {}
				})

			sqrt(sum)
		}
	}
}