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
            "org.apache.spark" % "spark-core_2.10" % "1.5.2",
			"org.scalanlp" % "breeze_2.10" % "0.11.2"
			)
		)