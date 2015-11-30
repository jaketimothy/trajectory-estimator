// Estimator.scala
package com.jaketimothy.estimator

import org.apache.spark._
import org.apache.spark.SparkContext._
import org.apache.spark.sql.SQLContext
// import java.nio.file.{Files, Paths}
// import org.json4s._
// import org.json4s.jackson.JsonMethods._
// import org.json4s.jackson.Serialization
// import org.json4s.jackson.Serialization.{read, writePretty}
import com.jaketimothy.estimator.planetmodel._
import breeze.linalg._

object EstimatorApp {
	def main(args: Array[String]) {
		// input: observation stations, observations
		// output: estimated trajectory

		// initialize Spark
		val conf = new SparkConf().setAppName("Trajectory Estimator")
		val sc = new SparkContext(conf)
		val sqlContext = new SQLContext(sc)
		import sqlContext.implicits._

		// read data (TODO : sanitize data?)
		val stationData = sqlContext.read.json(args(0))
		stationData.show()
		// val utf8 = java.nio.charset.StandardCharsets.UTF_8
		// implicit val formats = Serialization.formats(NoTypeHints)
		// val stationSource = io.Source.fromFile(args(0))
		// val stationsIn = read[List[StationInfo]](
		// 	try stationSource.getLines.mkString finally stationSource.close)
		// val stations = stationsIn.map(Station(_)).toVector
		val observationData = sqlContext.read.json(args(1))
		val outfilePath = args(2)

		// initialize models
		val earthHarmonicCoefficients = SphericalHarmonicGravityModel.parseWgs84CoefficientsFile("egmfile", 16) // TODO : how to specify egm file?
		
		// ode
		object MotionEquations extends FirstOrderDifferentialEquations {

			override val getDimension = 6

			val earth = new HarmonicWGS84Earth(earthHarmonicCoefficients);

			override def computeDerivatives(t: Double, y: Array[Double], yDot: Array[Double]): Unit = {
				val derivatives = y.slice(3, 6) ++ earth.gravityModel.gravitationalAcceleration(DenseVector(y.slice(0, 3))).toArray
				derivatives.copyToArray(yDot)
			}
		}

		// process
		// val batchProcessor = new UnscentedBatchEstimator(
		// 	MotionEquations, observationEquations, 0.15, Vector.fill(6)(0.15), Vector.fill(6)(0.15))
		//batchProcessor.estimate()
	}
}
