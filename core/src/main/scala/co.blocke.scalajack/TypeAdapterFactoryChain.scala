package co.blocke.scalajack

import scala.reflect.runtime.universe.{ Type, TypeTag }

object TypeAdapterFactoryChain {

  def apply(factories: List[TypeAdapterFactory]): TypeAdapterFactoryChain =
    factories match {
      case Nil =>
        new TypeAdapterFactoryChain {
          override def typeAdapter(tpe: Type, context: Context): Option[TypeAdapter[_]] = {
            None
          }
        }

      case head :: tail =>
        new TypeAdapterFactoryChain {
          override def typeAdapter(tpe: Type, context: Context): Option[TypeAdapter[_]] = {
            head.typeAdapter(tpe, context, TypeAdapterFactoryChain(tail))
          }
        }
    }

}

trait TypeAdapterFactoryChain {

  def typeAdapterOf[T](context: Context)(implicit typeTag: TypeTag[T]): Option[TypeAdapter[T]] = ???

  def typeAdapter(tpe: Type, context: Context): Option[TypeAdapter[_]]

}
