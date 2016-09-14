package co.blocke.scalajack.json
package typeadapter

import scala.reflect.runtime.universe.{ Type, typeOf }

object ListTypeAdapter extends TypeAdapterFactory {

  override def typeAdapter(tpe: Type, context: Context): Option[TypeAdapter[_]] =
    if (tpe <:< typeOf[List[_]]) {
      val elementType = tpe.dealias.typeArgs.head
      val elementTypeAdapter = context.typeAdapter(elementType)

      Some(CanBuildFromTypeAdapter(List.canBuildFrom, elementTypeAdapter))
    } else {
      None
    }

}
