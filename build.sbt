// build.sbt

lazy val commonSettings = Seq(
  organization := "com.jaketimothy",
  version := "0.1.0",
  scalaVersion := "2.10.6"
  )

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
	name := "trajectory-estimator",
	libraryDependencies ++= Seq(
	  "org.apache.commons" % "commons-math3" % "3.5",
      "org.apache.spark" % "spark-core_2.10" % "1.6.1",
      "org.apache.spark" % "spark-sql_2.10" % "1.6.1",
	    "org.scalanlp" % "breeze_2.10" % "0.12",
	    "org.scalatest" % "scalatest_2.10" % "2.2.6"
	  )
	)