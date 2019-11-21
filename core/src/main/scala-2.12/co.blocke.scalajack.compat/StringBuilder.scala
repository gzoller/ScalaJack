package co.blocke.scalajack.compat

import scala.collection.mutable

case class StringBuilder() extends mutable.Builder[String, String] {

  private val buf = new java.lang.StringBuilder()

  def +=(elem: String): StringBuilder.this.type = {
    buf.append(elem)
    this
  }

  def clear(): Unit = buf.setLength(0)

  def result(): String = buf.toString
}
