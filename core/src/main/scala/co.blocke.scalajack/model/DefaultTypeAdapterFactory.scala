package co.blocke.scalajack
package model

object DefaultTypeAdapterFactory extends TypeAdapterFactory {

  // This is the "end fo the chain".  Theoretically its impossible to see this--everything, even primitive types, are
  // classes, so the PlainClassTypeAdapter should be the practical end of the chain.  Just in case tho...
  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    throw new IllegalArgumentException(s"Unable to find a type adapter for ${tt.tpe}")

}