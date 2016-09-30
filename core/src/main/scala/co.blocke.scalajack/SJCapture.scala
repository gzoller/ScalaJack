package co.blocke.scalajack

import scala.collection.mutable.Map

trait SJCapture {
  private var captured = Map.empty[String, Any]

  def capture(forField: String, value: Any) = captured.put(forField, value)
  def regurgitate() = captured.toMap
}