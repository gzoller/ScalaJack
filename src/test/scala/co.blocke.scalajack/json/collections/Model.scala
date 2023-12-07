package co.blocke.scalajack
package json
package collections

import java.util.{ArrayList, Map as JMap, Set as JSet}

case class Person(name: String, age: Int)

case class SeqHolder[T](a: Seq[T])
case class SetHolder[T](a: Set[T])
case class ArrayHolder[T](a: Array[T])

case class MapHolder[T, V](a: Map[T, V])
case class MMapHolder[T, V](a: scala.collection.mutable.Map[T, V])
case class JMapHolder[T, V](a: JMap[T, V])

class Distance(val meters: Double) extends AnyVal

case class TupleHolder[A, B, C](a: (A, B, C))

case class ArrayListHolder[T](a: ArrayList[T])
case class JSetHolder[T](a: JSet[T])

enum Color:
  case Red, Green, Blue

object Permissions extends Enumeration {
  type Permissions = Value
  val READ, WRITE, EXEC, NONE = Value
}
import Permissions.*
