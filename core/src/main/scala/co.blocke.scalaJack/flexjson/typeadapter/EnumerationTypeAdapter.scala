package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{Context, TypeAdapter, TypeAdapterFactory}

import scala.reflect.runtime.universe.{ClassSymbol, Type, typeOf}

object EnumerationTypeAdapter extends TypeAdapterFactory.FromClassSymbol {

  override def typeAdapter(tpe: Type, classSymbol: ClassSymbol, context: Context): Option[TypeAdapter[_]] =
    if (tpe <:< typeOf[scala.Enumeration]) {
      None
    } else {
      None
    }

}
