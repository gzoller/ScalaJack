package co.blocke.scalajack

import scala.reflect.api.{ Mirror, Universe }
import scala.reflect.runtime.universe.TypeTag

object TypeTags {

  def of[T](m: scala.reflect.runtime.universe.Mirror, t: scala.reflect.runtime.universe.Type): TypeTag[T] =
    new TypeTag[T] {

      // $COVERAGE-OFF$Never used in our context
      override def in[U <: Universe with Singleton](otherMirror: Mirror[U]): U#TypeTag[T] = this.asInstanceOf[U#TypeTag[T]]
      // $COVERAGE-ON$

      override val mirror: scala.reflect.runtime.universe.Mirror = m

      override def tpe: scala.reflect.runtime.universe.Type = t

    }

}
