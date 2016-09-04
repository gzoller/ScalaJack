package co.blocke.scalajack.flexjson

import scala.reflect.runtime.universe.{ ClassSymbol, Type }

object TypeAdapterFactory {

  trait FromClassSymbol extends TypeAdapterFactory {

    override def typeAdapter(tpe: Type, context: Context, superParamTypes: List[Type]): Option[TypeAdapter[_]] = {
      val typeSymbol = tpe.typeSymbol
      if (typeSymbol.isClass) {
        typeAdapter(tpe, typeSymbol.asClass, context, superParamTypes)
      } else {
        None
      }
    }

    def typeAdapter(tpe: Type, classSymbol: ClassSymbol, context: Context, superParamTypes: List[Type]): Option[TypeAdapter[_]]

  }

}

trait TypeAdapterFactory {

  def typeAdapter(tpe: Type, context: Context, superParamTypes: List[Type]): Option[TypeAdapter[_]]

}
