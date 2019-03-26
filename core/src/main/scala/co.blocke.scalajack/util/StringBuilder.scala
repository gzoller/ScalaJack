package co.blocke.scalajack.util

import scala.collection.mutable.Builder

case class StringBuilder() extends Builder[String, String] {

  private val buf = new StringBuffer()

  def +=(elem: String) = {
    buf.append(elem)
    this
  }

  def clear() = buf.setLength(0)

  def result() = buf.toString()
}
