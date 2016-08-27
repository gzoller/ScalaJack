package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{Context, TypeAdapter, TypeAdapterFactory}

import scala.reflect.runtime.universe.{Type, typeOf}

object ListTypeAdapter extends TypeAdapterFactory {

  override def typeAdapter(tpe: Type, context: Context): Option[TypeAdapter[_]] =
    if (tpe <:< typeOf[List[_]]) {
      val elementType = tpe.typeArgs.head
      val elementTypeAdapter = context.typeAdapter(elementType)

      Some(CanBuildFromTypeAdapter(List.canBuildFrom, elementTypeAdapter))
    } else {
      None
    }

}
