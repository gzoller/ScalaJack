package co.blocke.series5

import scala.reflect.runtime.universe.TypeTag

trait JackFlavor[S] {
  def read[T](src: S)(implicit tt: TypeTag[T]): T
  def render[T](instance: T)(implicit tt: TypeTag[T]): S
}

class RenderException(msg: String) extends Exception(msg)
