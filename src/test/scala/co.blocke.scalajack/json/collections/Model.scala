package co.blocke.scalajack
package json
package collections

import java.util.{ArrayList, Map as JMap, Set as JSet}

case class Person(name: String, age: Int)

opaque type OnOff = Boolean
opaque type Counter = Short

case class SeqHolder[T](a: Seq[T])
case class SetHolder[T](a: Set[T])
case class MSeqHolder[T](a: scala.collection.mutable.Seq[T])
case class MSetHolder[T](a: scala.collection.mutable.Set[T])
case class VectorHolder[T](a: Vector[T])
case class IndexedSeqHolder[T](a: IndexedSeq[T])
case class IterableHolder[T](a: Iterable[T])
case class ArrayHolder[T](a: Array[T])
case class Holder[T](a: T)

case class MapHolder[T, V](a: Map[T, V])
case class MapHolder2[T, V](a: scala.collection.immutable.HashMap[T, V]) // specific
case class MapHolder3[T, V](a: scala.collection.immutable.SeqMap[T, V]) // specific
case class MapHolder4[T, V](a: scala.collection.immutable.TreeMap[T, V]) // open coersion
case class MMapHolder[T, V](a: scala.collection.mutable.Map[T, V]) // specific
case class MMapHolder2[T, V](a: scala.collection.mutable.HashMap[T, V]) // specific
case class MMapHolder3[T, V](a: scala.collection.mutable.SeqMap[T, V]) // open coersion
case class JMapHolder[T, V](a: JMap[T, V])

class Distance(val meters: Double) extends AnyVal

case class TupleHolder[A, B, C](a: (A, B, C))
case class TupleOneHolder[A](a: Tuple1[A])

case class ArrayListHolder[T](a: ArrayList[T])
case class JSetHolder[T](a: JSet[T])

enum Color:
  case Red, Green, Blue

object Permissions extends Enumeration {
  type Permissions = Value
  val READ, WRITE, EXEC, NONE = Value
}
