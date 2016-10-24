package co.blocke.scalajack

import scala.reflect.runtime.universe.{ Type, TypeTag }

object TypeAdapterFactoryChain {

  def apply(factories: List[TypeAdapterFactory]): TypeAdapterFactory =
    factories match {
      case Nil =>
        new TypeAdapterFactory {
          override def typeAdapter(tpe: Type, context: Context, next: TypeAdapterFactory): TypeAdapter[_] =
            next.typeAdapter(tpe, context)
        }

      case head :: tail => // FIXME are we passing the correct "next"?
        new TypeAdapterFactory {
          override def typeAdapter(tpe: Type, context: Context, next: TypeAdapterFactory): TypeAdapter[_] = {
            head.typeAdapter(tpe, context, next = TypeAdapterFactoryChain(tail))
          }
        }
    }

}
