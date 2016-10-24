package co.blocke.scalajack

import scala.reflect.runtime.universe.TypeTag

object TypeAdapterFactoryChain {

  def apply(factories: List[TypeAdapterFactory]): TypeAdapterFactory =
    factories match {
      case Nil =>
        new TypeAdapterFactory {
          override def typeAdapterOf[T: TypeTag](context: Context, next: TypeAdapterFactory): TypeAdapter[T] =
            next.typeAdapterOf[T](context)
        }

      case head :: tail => // FIXME are we passing the correct "next"?
        new TypeAdapterFactory {
          override def typeAdapterOf[T: TypeTag](context: Context, next: TypeAdapterFactory): TypeAdapter[T] = {
            head.typeAdapterOf[T](context, next = TypeAdapterFactoryChain(tail))
          }
        }
    }

}
