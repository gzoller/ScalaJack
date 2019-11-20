package co.blocke.scalajack
package model

import scala.reflect.runtime.universe._

object DefaultTypeAdapterFactory extends TypeAdapterFactory {

  // This is the "end fo the chain".  Theoretically its impossible to see this--everything, even primitive types, are
  // classes, so the PlainClassTypeAdapter should be the practical end of the chain.  Just in case tho...
  override def typeAdapterOf[T](
      next: TypeAdapterFactory
  )(implicit taCache: TypeAdapterCache, tt: TypeTag[T]): TypeAdapter[T] =
    throw new IllegalArgumentException(
      s"Unable to find a type adapter for ${tt.tpe} (may be abstract or a dependency of an abstract class)"
    )

}
