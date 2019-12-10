package co.blocke.series60
package delimited

import model.{ Context, TypeAdapter, TypeAdapterFactory }
import typeadapter.OptionTypeAdapter

object DelimitedOptionTypeAdapterFactory extends TypeAdapterFactory {
  def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (tt.tpe <:< typeOf[Option[_]]) {
      val elementType :: Nil = tt.tpe.baseType(tt.tpe.typeSymbol).typeArgs
      OptionTypeAdapter(context.typeAdapter(elementType), true).asInstanceOf[TypeAdapter[T]]
    } else
      next.typeAdapterOf[T]
}