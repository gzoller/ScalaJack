package co.blocke.series60

//import scala.collection.mutable.Map

trait SJCapture {
  var captured: Map[String, Any] = Map.empty[String, Any]
}

// Java classes should inherit this!
class SJCaptureJava extends SJCapture