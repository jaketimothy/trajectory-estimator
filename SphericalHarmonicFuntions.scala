// SphericalHarmonicFunctions.scala
package estimator.functions

import estimator.AssociatedLegendrePolynomials
import math._

class SphericalHarmonicFunctions(val degree:Int) {
	// Normalized spherical harmonic functions are defined by
	//     Ybar(n, m, t, l) = Pbar(n, abs(m), cos(t)) * cos(m * l),  m >= 0      (1)
	//     Ybar(n, m, t, l) = Pbar(n, abs(m), cos(t)) * sin(abs(m) * l),  m < 0  (2)

	require(degree >= 0, "degree must be non-negative.")

	val associatedLegendrePolynomials = AssociatedLegendrePolynomials(degree)

	def normalizedValue(order:Int, theta:Double, lambda:Double) = {
		order match {
			case m if (abs(m) <= degree) => {
				(if(m >= 0) cos(m * lambda) else sin(-m * lambda)) *
					associatedLegendrePolynomials.normalizedValue(abs(m), cos(theta))
			}
		} 
	}
}