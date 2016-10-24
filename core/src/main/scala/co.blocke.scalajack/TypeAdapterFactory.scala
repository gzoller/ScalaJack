package co.blocke.scalajack

import scala.reflect.runtime.universe.{ ClassSymbol, TypeTag }

object TypeAdapterFactory {

  def apply[V](typeAdapter: TypeAdapter[V])(implicit expectedTypeTag: TypeTag[V]): TypeAdapterFactory =
    new TypeAdapterFactory {
      override def typeAdapterOf[T](context: Context, next: TypeAdapterFactory)(implicit actualTypeTag: TypeTag[T]): TypeAdapter[T] =
        if (expectedTypeTag.tpe =:= actualTypeTag.tpe) {
          typeAdapter.asInstanceOf[TypeAdapter[T]]
        } else {
          next.typeAdapterOf[T](context)
        }
    }

  trait FromClassSymbol extends TypeAdapterFactory {

    override def typeAdapterOf[T](context: Context, next: TypeAdapterFactory)(implicit tt: TypeTag[T]): TypeAdapter[T] = {
      val typeSymbol = tt.tpe.typeSymbol
      if (typeSymbol.isClass) {
        typeAdapterOf[T](typeSymbol.asClass, context, next)
      } else {
        next.typeAdapterOf[T](context)
      }
    }

    def typeAdapterOf[T](classSymbol: ClassSymbol, context: Context, next: TypeAdapterFactory)(implicit typeTag: TypeTag[T]): TypeAdapter[T]

  }

}

trait TypeAdapterFactory {

  def typeAdapterOf[T](context: Context, next: TypeAdapterFactory = DefaultTypeAdapterFactory)(implicit typeTag: TypeTag[T]): TypeAdapter[T]

}
