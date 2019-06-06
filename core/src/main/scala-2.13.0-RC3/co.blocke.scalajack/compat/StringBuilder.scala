package co.blocke.scalajack.compat

import scala.collection.mutable.Builder

case class StringBuilder() extends Builder[String, String] {

  private val buf = new StringBuffer()

  def addOne(elem: String) = {
    buf.append(elem)
    this
  }

  def clear() = buf.setLength(0)

  def result() = buf.toString()
}
