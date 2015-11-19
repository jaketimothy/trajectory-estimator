// InputTest.scala
import java.nio.file.{Files, Paths}
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.{read, writePretty}
import estimator._
//import breeze.linalg._

object InputTest extends App {

	val utf8 = java.nio.charset.StandardCharsets.UTF_8

	implicit val formats = Serialization.formats(NoTypeHints)

	val stationSource = io.Source.fromFile(args(0))
	//val observationData = sqlContext.read.json(args(1))
	//val outfilePath = args(2)

	val stationsIn = read[List[StationInfo]](
		try stationSource.getLines.mkString finally stationSource.close)
	val stations = stationsIn.map(Station(_))
	// val stationsOut = List(
	// 	StationInfo(
	// 		List(-2516715.36114, -4653003.08089, 3551245.35929),
	// 		List(1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0),
	// 		List(0.0),
	// 		List(4.0))
	// 	)

	println(stationsIn)
	println(stations)
	// Files.write(Paths.get("TestOutput.json"), writePretty(stationsOut).getBytes(utf8))
}