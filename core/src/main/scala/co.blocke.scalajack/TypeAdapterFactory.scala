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

  def typeAdapter(tpe: Type, context: Context, rest: TypeAdapterFactoryChain): Option[TypeAdapter[_]] =
    typeAdapter(tpe, context).orElse(rest.typeAdapter(tpe, context))

  def typeAdapterOf[T](context: Context, rest: TypeAdapterFactoryChain)(implicit typeTag: TypeTag[T]): Option[TypeAdapter[T]] = ???

  def typeAdapter(tpe: Type, context: Context): Option[TypeAdapter[_]]

}
