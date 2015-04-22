// SphericalHarmonicFunctions.scala
package estimator
import estimator.AssociatedLegendrePolynomials
import math.{abs, cos, sin}

class SphericalHarmonicFunctions(degree:Int) {
	// Normalized spherical harmonic functions are defined by
	//     Ybar(n, m, t, l) = Pbar(n, abs(m), cos(t)) * cos(m * l),  m >= 0      (1)
	//     Ybar(n, m, t, l) = Pbar(n, abs(m), cos(t)) * sin(abs(m) * l),  m < 0  (2)

	require(degree >= 0, "degree must be non-negative.")

	private val legendrePolynomials = AssociatedLegendrePolynomials(degree)

	def normalizedValue(order:Int, theta:Double, lambda:Double) = {
		order match {
			case m if (abs(m) <= degree) => {
				val legendreValue = legendrePolynomials.normalizedValue(abs(m), cos(theta))
				if (m >= 0) {
					legendreValue * cos(m * lambda)
				} else {
					legendreValue * sin(-m * lambda)
				}
			}
		} 
	}
}