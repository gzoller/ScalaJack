package co.blocke.scalajack
package json

import writing.*
import reading.*

trait JsonCodec[A] {

  // def decodeValue(in: JsonReader, default: A): A =
  //   ${ genReadVal(rootTpe :: Nil, 'default, cfg.isStringified, false, 'in) }

  def encodeValue(in: A, out: JsonOutput): Unit
  def decodeValue(in: JsonSource): A
}
