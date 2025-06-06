package co.blocke.scalajack
package xml

import writing.*
import reading.*

trait XmlCodec[A] {
  def encodeValue(in: A, out: XmlOutput): Unit
  def decodeValue(in: XmlSource): A
}

