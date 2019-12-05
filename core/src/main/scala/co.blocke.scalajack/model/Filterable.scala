package co.blocke.scalajack
package model

import scala.reflect.runtime.universe._
import scala.util.{ Try, Success, Failure }

/**
 * This is a convenience to make a more Scala-like filtering experience, i.e. use extractors so filters
 * can be used in case statements.  Note this isn't super-efficient as each application of the filter
 * is an attempted re-parsing of the input, but in the scheme of things you'd likely have to do that
 * anyway.
 *
 * @tparam WIRE
 */
trait Filterable[WIRE] {

  this: JackFlavor[WIRE] =>

  case class Filter[T](ta: TypeAdapter[T], k: String, tpe: Type) {
    def unapply(parser: Parser): Option[T] = {
      parser.revertToMark(0)
      Try {
        if (k.nonEmpty) {
          if (parser.scanForHint(k, typeValueModifier) <:< tpe.typeArgs.head)
            ta.read(parser)
          else
            throw new Exception("boom")
        } else
          ta.read(parser)
      }.toOption
    }
  }

  def filter[T](
      typeFieldMemberName: String = ""
  )(implicit tt: TypeTag[T]): Filter[T] =
    Filter(
      taCache.typeAdapter(tt.tpe).asInstanceOf[TypeAdapter[T]],
      typeFieldMemberName,
      tt.tpe
    )
}
