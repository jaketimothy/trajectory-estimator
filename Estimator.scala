// Estimator.scala
package estimator

import org.apache.spark._
import org.apache.spark.SparkContext._
import org.apache.spark.sql.SQLContext

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

	// define ivp ode
	def motionEquations(t:Double, y:Vector[Double]) = {
		// dy = f(t, y)
	}
}
