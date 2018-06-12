package co.blocke

import scala.reflect.runtime.universe.{ Type, typeOf }

object TypeTagHacks {

  val TypeType: Type = typeOf[Type]

}
