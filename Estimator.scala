// Estimator.scala
package estimator

import org.apache.spark._
import org.apache.spark.SparkContext._
import org.apache.spark.sql.SQLContext
import estimator.planetmodel._
import org.apache.commons.math3.geometry.euclidean.threed.Vector3D

object Estimator extends App {
	// input: observation stations, observations
	// output: estimated trajectory

	// initialize Spark
	val conf = new SparkConf().setAppName("estimator")
	val sc = new SparkContext(conf)
	val sqlContext = new SQLContext(sc)

	// read data (TODO : sanitize data?)
	val stations = sqlContext.read.json(args(0))
	val observations = sqlContext.read.json(args(1))
	val outfilePath = args(2)

	// initialize models
	val earthHarmonicCoefficients = SphericalHarmonicGravityModel.parseWgs84CoefficientsFile("egmfile", 16) // TODO : how to specify egm file?
	val earth = Earth(Earth.EarthModelType.WGS84, earthHarmonicCoefficients)

	// define ivp ode
	def motionEquations(t:Double, y:(Vector3D,Vector3D)) = {
		// dy = f(t, y), y = [y, ydot]
		(y._2, earth.gravityModel.gravitationalAcceleration(y._1)
	}

	def observationEquations(x:Vector[Double]) = {

	}

	// process
	val batchProcessor = BatchProcessor(motionEquations, observationEquations, 0.15, Vector.fill(6)(0.15), Vector.fill(6)(0.15))
	batchProcessor.run()
}
