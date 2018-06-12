package co.blocke.scalajack
package typeadapter

/**
 * __DO NOT__ EXTEND THIS CLASS FOR TypeAdapter OVERRIDES FOR PRIMITIVE TYPES (e.g. in VisitorContext)!
 *
 * See note in BasicTypeAdapter for details.
 */
object SimpleTypeAdapter {

  abstract class ForTypeSymbolOf[V](implicit valueTypeTag: TypeTag[V]) extends TypeAdapterFactory with TypeAdapter[V] {

    override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
      if (tt.tpe.typeSymbol == valueTypeTag.tpe.typeSymbol) {
        this.asInstanceOf[TypeAdapter[T]]
      } else {
        next.typeAdapterOf[T]
      }

  }

}

@deprecated(message = "Usage `TypeAdapter.=:=[V]` instead", since = "7.0.0")
abstract class SimpleTypeAdapter[V: TypeTag] extends TypeAdapter.=:=[V]
