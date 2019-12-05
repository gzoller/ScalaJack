package co.blocke.scalajack
package delimited

import typeadapter.OptionTypeAdapter
import scala.reflect.runtime.universe._

import model._

/**
 * Options are handled a little differently for Delimited.  They should result in an empty field.
 * Empty fields area always read in as None, so no null fields are possible for Delimited options.
 */
object DelimitedOptionTypeAdapterFactory extends TypeAdapterFactory {
  def typeAdapterOf[T](
      next: TypeAdapterFactory
  )(implicit taCache: TypeAdapterCache, tt: TypeTag[T]): TypeAdapter[T] =
    if (tt.tpe <:< typeOf[Option[_]]) {
      val elementType :: Nil = tt.tpe.baseType(tt.tpe.typeSymbol).typeArgs
      OptionTypeAdapter(taCache.typeAdapter(elementType), nullIsNone = true)
        .asInstanceOf[TypeAdapter[T]]
    } else
      next.typeAdapterOf[T]
}
