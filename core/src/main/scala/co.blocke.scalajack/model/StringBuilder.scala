package co.blocke.scalajack
package model

import scala.collection.mutable

case class StringBuilder[WIRE]() extends mutable.Builder[WIRE, WIRE] {

  private val buf = new StringBuffer()

  def addOne(elem: WIRE): StringBuilder.this.type = {
    buf.append(elem)
    this
  }

  def clear() = buf.setLength(0)

  def result() = buf.toString.asInstanceOf[WIRE]
}
