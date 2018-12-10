package co.blocke.series5

import scala.reflect.runtime.universe.TypeTag

object DefaultTypeAdapterFactory extends TypeAdapterFactory {

  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    throw new IllegalArgumentException(s"Unable to find a type adapter for ${tt.tpe}")

}
