package co.blocke.scalajack

import scala.reflect.runtime.universe.Type

trait TypeAdapterFactoryChain {

  def typeAdapter(tpe: Type, context: Context): Option[TypeAdapter[_]]

}
