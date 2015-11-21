
// build.sbt

val math3 = "org.apache.commons" % "commons-math3" % "3.5"

lazy val commonSettings = Seq(
	organization := "com.jaketimothy",
	version := "0.1.0",
	scalaVersion := "2.10.6"
	)

lazy val root = (project in file(".")).
	settings(commonSettings: _*).
	settings(
		name := "trajectory-estimator",
		libraryDependencies += math3
		)