package co.blocke.scalajack

import scala.reflect.runtime.universe.{ ClassSymbol, Type, TypeTag }

object TypeAdapterFactory {

  trait FromClassSymbol extends TypeAdapterFactory {

    override def typeAdapter(tpe: Type, context: Context, next: TypeAdapterFactory): Option[TypeAdapter[_]] = {
      val typeSymbol = tpe.typeSymbol
      if (typeSymbol.isClass) {
        typeAdapter(tpe, typeSymbol.asClass, context, next)
      } else {
        next.typeAdapter(tpe, context)
      }
    }

    def typeAdapter(tpe: Type, classSymbol: ClassSymbol, context: Context, next: TypeAdapterFactory): Option[TypeAdapter[_]]

  }

}

trait TypeAdapterFactory {

  def typeAdapter(tpe: Type, context: Context, next: TypeAdapterFactory = DefaultTypeAdapterFactory): Option[TypeAdapter[_]] =
    typeAdapter(tpe, context).orElse(next.typeAdapter(tpe, context))

//  def typeAdapter(tpe: Type, context: Context): Option[TypeAdapter[_]]

}
