// Estimator.scala
package estimator

import org.apache.spark._
import org.apache.spark.SparkContext._
import org.apache.spark.sql.SQLContext
import estimator.planetmodel._
import breeze.linalg._

object Estimator extends App {
	// input: observation stations, observations
	// output: estimated trajectory

	// initialize Spark
	val conf = new SparkConf().setAppName("estimator")
	val sc = new SparkContext(conf)
	val sqlContext = new SQLContext(sc)

	// read data (TODO : sanitize data?)
	val stationData = sqlContext.read.json(args(0))
	val observationData = sqlContext.read.json(args(1))
	val outfilePath = args(2)

	// initialize models
	val earthHarmonicCoefficients = SphericalHarmonicGravityModel.parseWgs84CoefficientsFile("egmfile", 16) // TODO : how to specify egm file?
	val earth = Earth(Earth.EarthModelType.WGS84, earthHarmonicCoefficients)

	// ivp ode
	def motionEquations(y: DenseVector[Double], t: Double) = {
		// dy = f(y, t), y = [y, ydot]
		
		y(3 to 5) ++ earth.gravityModel.gravitationalAcceleration(y(0 to 2))
	}

	val stations: Vector[Station] = stationData.map(_ match {
		case s if (s.bias.length == 1) => RangeStation(s)
		case s if (s.bias.length == 3) => RAEStation(s)
		}).toVector

	// process
	val batchProcessor = UnscentedBatchEstimator(
		motionEquations, observationEquations, 0.15, Vector.fill(6)(0.15), Vector.fill(6)(0.15))
	batchProcessor.run()
}
