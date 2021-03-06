// Station.scala
package com.jaketimothy.estimator

import breeze.linalg.{DenseVector, DenseMatrix, *, norm}
import scala.math.{atan2, atan, Pi}

case class StationInfo(
	location: Array[Double],
	locationUncertaintyCovariance: Array[Double],
	bias: Array[Double],
	biasUncertaintyCovariance: Array[Double]
	)

abstract class Station(info: StationInfo) {
	// ECEF coordinates

	assume(info.location.size == 3)
	assume(info.locationUncertaintyCovariance.size == 9)
	assume(info.bias.size * info.bias.size == info.biasUncertaintyCovariance.size)
	
	val location = DenseVector(info.location)
	val locationUncertaintyCovariance = new DenseMatrix(3, 3,
		info.locationUncertaintyCovariance)
	val bias = DenseVector(info.bias)
	val biasUncertaintyCovariance = new DenseMatrix(info.bias.size, info.bias.size,
		info.biasUncertaintyCovariance)

	def observationUncertaintyCovariance: DenseMatrix[Double]

	// assumes state is in positionVector ++ velocityVector order
	def observationFromState(state: DenseVector[Double], t: Double): DenseVector[Double]
}

object Station {

	def apply(info: StationInfo): Station = info.bias.length match {
		case 1 => RangeStation(info)
		case 3 => RAEStation(info)
	}

	def apply(
		location: Array[Double],
		locationUncertaintyCovariance: Array[Double],
		bias: Array[Double],
		biasUncertaintyCovariance: Array[Double]
		): Station = {
		apply(StationInfo(
			location,
			locationUncertaintyCovariance,
			bias,
			biasUncertaintyCovariance))
	}
}

case class RangeStation(info: StationInfo) extends Station(info) {

	assume(bias.size == 1)

	override val observationUncertaintyCovariance = {

		biasUncertaintyCovariance + math.norm(locationUncertaintyCovariance)
	}

	override def observationFromState(state: DenseVector[Double], t: Double) = {

		require(state.size >= 3)

		DenseVector(norm(state(0 to 2) - location)) + bias
	}
}

case class RAEStation(info: StationInfo) extends Station(info) {
	// only positive values for azimuth

	assume(bias.size == 3)

	override val observationUncertaintyCovariance = {

		val alpha = 1.0
		val weights = UnscentedTransformation.weights(3, alpha)
		val scale = UnscentedTransformation.scaleFactor(alpha)
		val chi = UnscentedTransformation.sigmaPoints(
			location,
			locationUncertaintyCovariance,
			scale)
		val gamma = chi.map(observationFromState(_, 0.0))
		val y = (for (i <- 0 to gamma.length) yield weights._1(i) * gamma(i)).reduce(_ + _)
		val uncertaintyCovariance = (for (i <- 0 to gamma.length) yield {
			val dY = gamma(i) - y
			weights._2(i) * dY * dY.t
		}).reduce(_ + _)
		biasUncertaintyCovariance + uncertaintyCovariance
	}

	override def observationFromState(state: DenseVector[Double], t: Double) = {

		require(state.size >= 3)

		val r = state(0 to 2) - location
		val a = -atan2(r(1), r(0))
		val e = atan(r(2) / norm(r(0 to 1)))
		DenseVector(norm(r), if (a > 0.0) a else 2.0 * Pi + a, e) + bias
	}
}