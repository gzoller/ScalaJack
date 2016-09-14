package co.blocke.scalajack.json

import scala.reflect.runtime.universe.Type
import scala.reflect.runtime.currentMirror

object StaticClassFunction extends BijectiveFunction[String, Type] {

  override def apply(fullName: String): Type = currentMirror.staticClass(fullName).info

  override def unapply(tpe: Type): String = tpe.typeSymbol.fullName

}
