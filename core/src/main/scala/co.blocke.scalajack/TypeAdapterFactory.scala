package co.blocke.scalajack

import scala.reflect.runtime.universe.{ ClassSymbol, Type, TypeTag }

object TypeAdapterFactory {

  trait FromClassSymbol extends TypeAdapterFactory {

    override def typeAdapter(tpe: Type, context: Context): Option[TypeAdapter[_]] = {
      val typeSymbol = tpe.typeSymbol
      if (typeSymbol.isClass) {
        typeAdapter(tpe, typeSymbol.asClass, context)
      } else {
        None
      }
    }

    def typeAdapter(tpe: Type, classSymbol: ClassSymbol, context: Context): Option[TypeAdapter[_]]

  }

}

trait TypeAdapterFactory {

  def typeAdapter(tpe: Type, context: Context, next: TypeAdapterFactoryChain): Option[TypeAdapter[_]] =
    typeAdapter(tpe, context).orElse(next.typeAdapter(tpe, context))

  def typeAdapterOf[T](context: Context, next: TypeAdapterFactoryChain)(implicit typeTag: TypeTag[T]): Option[TypeAdapter[T]] = ???

  def typeAdapter(tpe: Type, context: Context): Option[TypeAdapter[_]]

}
