package co.blocke.series5

import scala.collection.mutable.Map

trait SJCapture {
  var captured: Map[String, Any] = Map.empty[String, Any]
}