package co.blocke.scalajack

import scala.reflect.runtime.universe.TypeTag

object TypeAdapterFactoryChain {

  def apply(factories: List[TypeAdapterFactory]): TypeAdapterFactory =
    factories match {
      case Nil =>
        new TypeAdapterFactory {
          override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
            next.typeAdapterOf[T]
        }

      case head :: tail => // FIXME are we passing the correct "next"?
        new TypeAdapterFactory {
          override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] = {
            head.typeAdapterOf[T](next = TypeAdapterFactoryChain(tail))
          }
        }
    }

}
