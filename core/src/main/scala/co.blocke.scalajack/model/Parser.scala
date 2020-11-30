package co.blocke.scalajack
package model

import typeadapter.classes.ClassTypeAdapterBase
import scala.collection.mutable
import co.blocke.scala_reflection.info.TypeMemberInfo

trait Parser {
  type WIRE

  val jackFlavor: JackFlavor[WIRE] // This is needed and used by permissive type adapters

  def expectString(nullOK: Boolean = true): String
  def expectList[K, TO](
      KtypeAdapter: TypeAdapter[K],
      builder:      mutable.Builder[K, TO]): TO
  def expectTuple(
      tupleFieldTypeAdapters: List[TypeAdapter[_]]
  ): List[Object]
  def expectMap[K, V, TO](
      keyTypeAdapter:   TypeAdapter[K],
      valueTypeAdapter: TypeAdapter[V],
      builder:          mutable.Builder[(K, V), TO]): TO
  def expectObject(
      classBase: ClassTypeAdapterBase[_],
      hintLabel: String
  ): (mutable.BitSet, List[Object], java.util.HashMap[String, _])
  def expectBoolean(): Boolean
  def expectNumber(nullOK: Boolean = false): String
  def peekForNull: Boolean // peek-ahead to find null
  def scanForHint(hint: String, converterFn: HintBijective): Class[_]

  // For embedded type members.  Convert the type member into runtime "actual" type, e.g. T --> Foo
  def resolveTypeMembers(
      typeMembersByName: Map[String, TypeMemberInfo],
      converterFn:       HintBijective
  ): Map[String, TypeMemberInfo] // Returns Map[Type Signature Type (e.g. 'T'), Type]

  def showError(msg: String): String
  def backspace(): Unit
  def mark(): Int
  def revertToMark(mark: Int): Unit
  def nextIsString: Boolean
  def nextIsNumber: Boolean
  def nextIsObject: Boolean
  def nextIsArray: Boolean
  def nextIsBoolean: Boolean
  def subParser(input: WIRE): Parser
  def sourceAsString: String
}