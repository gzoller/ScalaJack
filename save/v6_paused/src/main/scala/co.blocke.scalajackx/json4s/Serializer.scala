package co.blocke.scalajackx
package json4s

import org.json4s.native.JsonMethods._

trait Serializer {

  type WIRE

  def parseToPrimitive(wire: WIRE): Any
}

case class JsonSerializer() extends Serializer {
  type WIRE = String
  def parseToPrimitive(wire: WIRE): Any = parse(wire).values
}

trait TypeAdapter[T] {
  def read(primitive: Any): T
}

case class IntTypeAdapter() extends TypeAdapter[Int] {
  def read(primitive: Any): Int = primitive match {
    case b: BigInt => b.intValue()
    case _         => throw new Exception("Boom... needed an Int here")
  }
}

case class ListTypeAdapter[E](elementTypeAdapter: TypeAdapter[E]) extends TypeAdapter[List[E]] {
  def read(primitive: Any): List[E] = primitive match {
    case e: List[E] => e.map(elementTypeAdapter.read(_))
    case x          => throw new Exception(s"$x Boom... needed an Array here")
  }
}
