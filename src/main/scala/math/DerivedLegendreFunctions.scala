// DerivedLegendreFunctions.scala
package com.jaketimothy.estimator.math

import math.sqrt
import org.apache.commons.math3.analysis.polynomials.PolynomialFunction

/*
 * Legendre functions underpin the spherical harmonic functions. This implementation
 * uses the derived Legendre functions as described in [Jones 2010] Section 2.1.1.
 *
 *  References:
 *   [Jones 2010] Efficient Models for the Evaluation and Estimation of the Gravity Field.
 *     http://ccar.colorado.edu/geryon/papers/Misc/bajones_phd.pdf
 */

object DerivedLegendreFunctions {

	private val cachedB = collection.mutable.Map[(Int, Int), Double]()

	// [Jones 2010] Equation 2.9
	def getB(n: Int, m: Int): Double = cachedB.getOrElseUpdate((n, m),
		sqrt((2.0 * n + 1.0) * (2.0 * n - 1.0) / (n + m) / (n - m)))

	private val cachedA = collection.mutable.ArrayBuffer(
		Vector(new PolynomialFunction(Array(1.0))),
		Vector(new PolynomialFunction(Array(0.0, sqrt(3))), new PolynomialFunction(Array(sqrt(3)))))

	def normalizedSet(degree: Int): Vector[PolynomialFunction] = degree match {
		// [Jones 2010] Equation 2.11

		case n if (n == 0 || n == 1) => cachedA(n)
		case n if (n >= 2) => {
			if (n > cachedA.length - 1) {
				(cachedA.length - 1 to n).foreach(d => {
					val aLess1 = cachedA(d - 1)
					val aLess2 = cachedA(d - 2)
					val aTemp = (0 to d - 1).map(m => {
						aLess1(m).multiply(new PolynomialFunction(Array(0.0, getB(d, m))))
						.subtract(aLess2(m).multiply(new PolynomialFunction(Array(getB(d, m) / getB(d - 1, m)))))
					}).toVector
					cachedA += aTemp :+ aLess1(d - 1).multiply(new PolynomialFunction(Array(sqrt((2.0 * d + 1.0) / (2.0 * d)))))
				})
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
