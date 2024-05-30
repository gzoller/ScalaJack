package co.blocke.scalajack
package json

import writing.*
import reading.*

trait JsonCodec[A] {
  def encodeValue(in: A, out: JsonOutput): Unit
  def decodeValue(in: JsonSource): A
}
