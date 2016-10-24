package co.blocke.scalajack

import scala.reflect.runtime.universe.{ ClassSymbol, TypeTag }

object TypeAdapterFactory {

  def apply[V](typeAdapter: TypeAdapter[V])(implicit expectedTypeTag: TypeTag[V]): TypeAdapterFactory =
    new TypeAdapterFactory {
      override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, actualTypeTag: TypeTag[T]): TypeAdapter[T] =
        if (expectedTypeTag.tpe =:= actualTypeTag.tpe) {
          typeAdapter.asInstanceOf[TypeAdapter[T]]
        } else {
          next.typeAdapterOf[T]
        }
    }

  trait FromClassSymbol extends TypeAdapterFactory {

    override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] = {
      val typeSymbol = tt.tpe.typeSymbol
      if (typeSymbol.isClass) {
        typeAdapterOf[T](typeSymbol.asClass, next)
      } else {
        next.typeAdapterOf[T]
      }
    }

    def typeAdapterOf[T](classSymbol: ClassSymbol, next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T]

  }

}

trait TypeAdapterFactory {

  def typeAdapterOf[T](implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    typeAdapterOf[T](DefaultTypeAdapterFactory)

  def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T]

}
