package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{ Context, TypeAdapter, TypeAdapterFactory }

import scala.reflect.runtime.universe.{ Type, typeOf }

object SetTypeAdapter extends TypeAdapterFactory {

  override def typeAdapter(tpe: Type, context: Context, superParamTypes: List[Type]): Option[TypeAdapter[_]] =
    if (tpe <:< typeOf[Set[_]]) {
      val elementType = tpe.typeArgs.head
      val elementTypeAdapter = context.typeAdapter(elementType, elementType.typeArgs)

      Some(CanBuildFromTypeAdapter(Set.canBuildFrom, elementTypeAdapter))
    } else {
      None
    }

}
