// DerivedLegendreFunctions.scala
package estimator.functions

import math.sqrt
import org.apache.commons.math3.analysis.polynomials.PolynomialFunction

object DerivedLegendreFunctions {

	private val cachedB = collection.mutable.Map[(Int, Int), Double]()

	// Jones, equation 2.9
	def getB(n: Int, m: Int): Double = cachedB.getOrElseUpdate((n, m),
		sqrt((2.0 * n + 1.0) * (2.0 * n - 1.0) / (n + m) / (n - m)))

	private val cachedA = collection.mutable.ArrayBuffer(
		Vector(new PolynomialFunction(Array(1.0))),
		Vector(new PolynomialFunction(Array(0.0, sqrt(3))), new PolynomialFunction(Array(sqrt(3)))))

	def normalizedSet(degree: Int): Vector[PolynomialFunction] = degree match {
		// Jones, equation 2.11

		case n if (n == 0 || n == 1) => cachedA(n)
		case n if (n >= 2) => {
			if (n > cachedA.length - 1) {
				(cachedA.length - 1 to n).foreach{
					val aLess1 = cachedA(_ - 1)
					val aLess2 = cachedA(_ - 2)
					val aTemp = (0 to _ - 1).map(m => 
						aLess1(m).multiply(new PolynomialFunction(Array(0.0, getB(_, m))))
						.subtract(aLess2(m).multiply(new PolynomialFunction(Array(getB(_, m) / getB(_ - 1, m))))))
					cachedA += aTemp :+ aLess1(_ - 1).multiply(new PolynomialFunction(Array(sqrt((2.0 * _ + 1.0) / (2.0 * _)))))
				}
			}
			cachedA(n)
		}
	}

	def normalized(degree: Int, order: Int): PolynomialFunction = normalizedSet(degree)(order)

	def normalizedValue(degree: Int, order: Int, x: Double): Double = (degree, order) match {
		case (n, m) if (n >= 0 && m <= n) => normalizedSet(degree)(order).value(x)
		case (n, m) if (n >= 0 && m > n) => 0.0
	}
}
