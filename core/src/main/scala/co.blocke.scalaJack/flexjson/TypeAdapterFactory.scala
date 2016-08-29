package co.blocke.scalajack.flexjson

import scala.reflect.runtime.universe.{ClassSymbol, Type}

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

  def typeAdapter(tpe: Type, context: Context): Option[TypeAdapter[_]]

}
