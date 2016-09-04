package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{ Context, TypeAdapter, TypeAdapterFactory }

import scala.reflect.runtime.universe.{ Type, TypeTag }

abstract class SimpleTypeAdapter[T](implicit valueTypeTag: TypeTag[T]) extends TypeAdapterFactory with TypeAdapter[T] {

  val valueType = valueTypeTag.tpe

  override def typeAdapter(tpe: Type, context: Context, superParamTypes: List[Type] = List.empty[Type]): Option[TypeAdapter[_]] =
    if (tpe =:= valueType) {
      Some(this)
    } else {
      None
    }

}
