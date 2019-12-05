package co.blocke.series60.util

import scala.reflect.runtime.universe.{ Type, typeOf }

object TypeTagHacks {

  val TypeType: Type = typeOf[Type]

}
