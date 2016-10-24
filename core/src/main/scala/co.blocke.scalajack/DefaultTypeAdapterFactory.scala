package co.blocke.scalajack

import scala.reflect.runtime.universe.Type

object DefaultTypeAdapterFactory extends TypeAdapterFactory {

  override def typeAdapter(tpe: Type, context: Context, next: TypeAdapterFactory): TypeAdapter[_] =
    throw new IllegalArgumentException(s"Unable to find a type adapter for $tpe")

}
