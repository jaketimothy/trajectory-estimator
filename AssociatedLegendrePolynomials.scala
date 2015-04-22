// AssociatedLegendrePolynomials.scala
package estimator
import math.{pow, abs, sqrt, BigDecimal}
import org.apache.commons.math3.analysis.polynomials.{PolynomialFunction, PolynomialsUtils}

class AssociatedLegendrePolynomials(degree:Int) {
	// Associated legendre polynomials are defined by
	//     P(n, m, x) = (-1)^m * (1 - x^2)^(m/2) * d^m/dx^m(P(n, x)),  m >= 0       (1)
	//     P(n, m, x) = (-1)^m * (n + m)!/(n - m)! * P(n, abs(m), x),  m < 0        (2)
	// where
	//     P(n, x) = LegendrePolynomial(n).value(x)                                 (3)
	// Fully normalized associated legendre polynomials are defined by
	//     Pbar(n, m, x) = (-1)^m * sqrt((n + 1/2)*(n - m)!/(n + m)!) * P(n, m, x)  (4)
	// which, taken with (1) and (2), results in
	//     Pbar(n, m, x) = sqrt((n + 1/2)*(n - m)!/(n + m)!) * (1 - x^2)^(m/2) * d^m/dx^m(P(n, x)),  m >= 0  (5)
	//     Pbar(n, m, x) = (-1)^m * Pbar(n, abs(m), x),  m < 0                      (6)
	
	require(degree >= 0, "degree must be non-negative.")

	private lazy val factorial = {
		(1 to 2 * degree).scanLeft(BigDecimal.apply(1.0))((b, n) => b * m)
	}

	private val normalizedPolynomials = {
		val originalFunction = PolynomialsUtils.createLegendrePolynomial(degree).multiply(
			new PolynomialFunction(Array(sqrt(degree + 0.5)))
			)
		(1 to degree).scanLeft(originalFunction)((b, m) => {
				b.polynomialDerivative.multiply(
					new PolynomialFunction(Array(1.0 / ((degree + m) * (degree - m + 1.0))))
					)
				})
	}

	def value(order:Int, x:Double) = {
		order match {
			case m if (abs(m) <= degree) => {
				val factorialRatio = sqrt((factorial(degree + m) / factorial(degree - m))).toDouble
				val value = normalizedValue(m, x) / sqrt(degree + 0.5)
				if (m > 0) {
					value * factorialRatio * pow(-1.0, m % 2)
				} else {
					value / factorialRatio
				}
			}
		}
	}

	def normalizedValue(order:Int, x:Double) = {
		order match {
			case m if (abs(m) <= degree) => {
				val value = pow(1.0 - x * x, abs(m) / 2.0) * normalizedPolynomials(abs(m)).value(x)
				if (m < 0 && m % 2 == 1) {
					-value
				} else {
					value
				}
			}
		}
	}
}
