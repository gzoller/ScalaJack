package co.blocke.scalajack
package model

trait Transceiver[WIRE] extends Reader[WIRE] with Writer[WIRE] {
  val jackFlavor: JackFlavor[_, WIRE]
}
