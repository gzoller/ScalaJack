package co.blocke.scalajack

import scala.reflect.runtime.universe.TypeTag

object DefaultTypeAdapterFactory extends TypeAdapterFactory {

  override def typeAdapterOf[T](context: Context, next: TypeAdapterFactory)(implicit tt: TypeTag[T]): TypeAdapter[T] =
    throw new IllegalArgumentException(s"Unable to find a type adapter for ${tt.tpe}")

}
