package co.blocke.scalajack.flexjson

import scala.reflect.runtime.universe.Type

trait TypeAdapterFactory {

  def typeAdapter(tpe: Type, context: Context): Option[TypeAdapter[_]]

}
