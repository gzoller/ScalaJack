package co.blocke.scalajack

import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror
import PrimitiveTypes._

// Behold, the sublime power of the Cake Pattern!
// The wiring is accomplished in the ScalaJack trait.

// The weird typing ('S here) is to allow for the fact that future
// serializations may be to non-String output, e.g. bytes, or Mongo Document.

trait JackFlavor[S] {
  def rr: ReadRenderer
  trait ReadRenderer {
    def read[T](src: S)(implicit tt: TypeTag[T], vc: VisitorContext = VisitorContext()): T
    def render[T](instance: T)(implicit tt: TypeTag[T], vc: VisitorContext = VisitorContext()): S

    protected def clean(input: String): String = {
      val buffer = new StringBuffer(input.length())
      for (i <- 0 to input.length - 1) {
        if (input.charAt(i) > 256) {
          val hex = Integer.toHexString(input.charAt(i))
          buffer.append("\\u").append(hex.reverse.padTo(4, "0").reverse.mkString)
        } else buffer.append(input.charAt(i) match {
          case '\n' => "\\n"
          case '\t' => "\\t"
          case '\r' => "\\r"
          case '\b' => "\\b"
          case '\f' => "\\f"
          case '\"' => "\\\""
          case '\\' => "\\\\"
          case c    => c
        })
      }
      buffer.toString
    }
  }
}

class RenderException(msg: String) extends Exception(msg)
