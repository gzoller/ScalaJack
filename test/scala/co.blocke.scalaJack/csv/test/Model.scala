package co.blocke.scalajack
package csv
package test

import java.util.UUID

object Size extends Enumeration {
  val Small, Medium, Large = Value
}
case class BasicScala(bd: BigDecimal, bi: BigInt, b: Boolean, bt: Byte, c: Char, d: Double, e: Size.Value, f: Float, l: Long, s: Short, str: String, u: UUID)
case class HasAny(a1: Any, a2: Any, a3: Any)
case class Strings(s1: String, s2: String, s3: String)
case class Thing(name: String, num: Int)
case class Nested(id: String, item: Thing)
case class Nested2(id: String, item: List[Thing])

case class Maybe(s: String, dunno: Option[String], more: Boolean)
case class VC(s: String) extends AnyVal
case class WithVC(id: Int, name: VC, isOk: Boolean)