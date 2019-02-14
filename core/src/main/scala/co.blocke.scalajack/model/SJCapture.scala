package co.blocke.scalajack
package model

//import scala.collection.mutable.Map

trait SJCapture {
  var captured: Map[String, Any] = Map.empty[String, Any]
}

// Java classes should inherit this!
class SJCaptureWrapper extends SJCapture