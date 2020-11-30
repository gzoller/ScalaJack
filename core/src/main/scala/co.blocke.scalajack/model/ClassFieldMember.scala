package co.blocke.scalajack
package model

import co.blocke.scala_reflection.info._
import typeadapter._

case class ClassFieldMember[OWNER,T](
  info:                               FieldInfo,
  valueTypeAdapter:                   TypeAdapter[T],
  outerClass:                         java.lang.Class[OWNER],  // class that "owns" this field
  dbKeyIndex:                         Option[Int],
  fieldMapName:                       Option[String]
):
  def name: String = fieldMapName.getOrElse(info.name)
  lazy val isOptional: Boolean = valueTypeAdapter match {
    case _: OptionTypeAdapter[_] => true
    case _: JavaOptionalTypeAdapter[_] => true
    case _ if info.annotations.contains(OPTIONAL_ANNO) => true
    case _ => false
  }