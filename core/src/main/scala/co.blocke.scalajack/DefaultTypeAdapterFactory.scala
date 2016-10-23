package co.blocke.scalajack

import scala.reflect.runtime.universe.Type

object DefaultTypeAdapterFactory extends TypeAdapterFactory {

  override def typeAdapter(tpe: Type, context: Context, next: TypeAdapterFactory): Option[TypeAdapter[_]] =
    None

}
