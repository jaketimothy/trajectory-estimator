// Estimator.scala
package estimator

import org.apache.spark._
import org.apache.spark.SparkContext._
import org.apache.spark.sql.SQLContext

object Estimator extends App {
	// takes as input: observation stations, observations, on-board data
	// output: estimated trajectory

	// initialize Spark
	val conf = new SparkConf().setAppName("estimator")
	val sc = new SparkContext(conf)
	val sqlContext = new SQLContext(sc)

	// read data
	val stations = sqlContext.read.json(args(0))
	val observations = sqlContext.read.json(args(1))
	val onboardData = sqlContext.read.json(args(2))

	val outfilePath = args(3)
}
