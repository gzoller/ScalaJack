package co.blocke.scalajack
package typeadapter

import scala.reflect.runtime.universe.{ Type, typeOf }

@deprecated(message = "Use `CanBuildFromTypeAdapter` object", since = "2016-09-26")
object SetTypeAdapter extends TypeAdapterFactory {

  override def typeAdapter(tpe: Type, context: Context): Option[TypeAdapter[_]] =
    if (tpe <:< typeOf[Set[_]]) {
      val elementType = tpe.typeArgs.head
      val elementTypeAdapter = context.typeAdapter(elementType)

      Some(CanBuildFromTypeAdapter(Set.canBuildFrom, elementTypeAdapter))
    } else {
      None
    }

}
