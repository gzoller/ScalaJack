package co.blocke.scalajack
package typeadapter

import model._

import scala.collection.mutable
import scala.reflect.runtime.universe._

trait ClassTypeAdapterBase[T] {
  val typeMembersByName: Map[String, ClassHelper.TypeMember[T]]
  val fieldMembersByName: Map[String, ClassHelper.ClassFieldMember[T, Any]]
  val argsTemplate: Array[Any]
  val fieldBitsTemplate: mutable.BitSet
  val isSJCapture: Boolean
  val dbCollectionName: Option[String]

  // This one's for read().  Use Parser to marshal type member values into Type objects
  def substituteTypeMemberTypes(
      parser:  Parser,
      taCache: TypeAdapterCache
  ): Map[String, ClassHelper.ClassFieldMember[T, Any]] = {
    val foundByParser = parser.resolveTypeMembers(
      typeMembersByName,
      taCache.jackFlavor.typeValueModifier
    )
    if (typeMembersByName.size != foundByParser.size)
      throw new ScalaJackError(
        parser.showError(
          "Did not find required type member(s): " + typeMembersByName.keySet
            .diff(foundByParser.keySet.map(_.toString))
            .mkString(",")
        )
      )
    substituteTypeMemberTypes(foundByParser, taCache)
  }

  def substituteTypeMemberTypes(
      typeMemberToActualType: Map[Type, Type],
      taCache:                TypeAdapterCache
  ): Map[String, ClassHelper.ClassFieldMember[T, Any]] = {
    fieldMembersByName.map {
      case (name, fm) =>
        val revisedFieldMember: ClassHelper.ClassFieldMember[T, Any] =
          typeMemberToActualType
            .get(fm.declaredValueType)
            .map { actualType =>
              val actualTypeAdapter = taCache.typeAdapter(actualType)
              val newValueTypeAdapter = fm.valueTypeAdapter match {
                case fallback: FallbackTypeAdapter[_, _] =>
                  FallbackTypeAdapter(
                    Some(actualTypeAdapter.asInstanceOf[TypeAdapter[Any]]),
                    fallback.orElseTypeAdapter
                  )
                case _ => actualTypeAdapter
              }
              fm.copy(
                declaredValueType = actualType,
                valueTypeAdapter  = newValueTypeAdapter
              )
                .asInstanceOf[ClassHelper.ClassFieldMember[T, Any]]
            }
            .getOrElse(fm)
        (name, revisedFieldMember)
    }
  }

  def dbKeys: List[ClassHelper.ClassFieldMember[T, Any]] =
    fieldMembersByName.values.toList
      .filter(_.dbKeyIndex.isDefined)
      .sortBy(_.dbKeyIndex.get)
}
