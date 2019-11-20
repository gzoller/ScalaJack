package co.blocke.scalajack
package util

import scala.reflect.api.Universe
import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._

object TypeTags {

  def of[T](m: Mirror, t: Type): TypeTag[T] =
    new TypeTag[T] {

      // $COVERAGE-OFF$Never used in our context
      override def in[U <: Universe with Singleton](
          otherMirror: scala.reflect.api.Mirror[U]
      ): U#TypeTag[T] = this.asInstanceOf[U#TypeTag[T]]
      // $COVERAGE-ON$

      override val mirror: Mirror = m

      override def tpe: Type = t

    }

  def of[T](t: Type): TypeTag[T] = {
    new TypeTag[T] {
      // $COVERAGE-OFF$Unused in our context
      override def in[U <: Universe with Singleton](
          otherMirror: scala.reflect.api.Mirror[U]
      ): U#TypeTag[T] = this.asInstanceOf[U#TypeTag[T]]
      // $COVERAGE-ON$
      override val mirror: universe.Mirror =
        scala.reflect.runtime.currentMirror
      override def tpe: universe.Type = t
    }
  }
}
