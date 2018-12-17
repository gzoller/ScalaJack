package co.blocke.scalajack
package model

object DefaultTypeAdapterFactory extends TypeAdapterFactory {

  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter =
    throw new IllegalArgumentException(s"Unable to find a type adapter for ${tt.tpe}")

}