package co.blocke.scalajack

import scala.reflect.api.Universe
import scala.reflect.runtime.{ currentMirror, universe }

object TypeTags {

  def of[T](m: scala.reflect.runtime.universe.Mirror, t: Type): TypeTag[T] =
    new TypeTag[T] {

      // $COVERAGE-OFF$Never used in our context
      override def in[U <: Universe with Singleton](otherMirror: scala.reflect.api.Mirror[U]): U#TypeTag[T] = this.asInstanceOf[U#TypeTag[T]]
      // $COVERAGE-ON$

      override val mirror: scala.reflect.runtime.universe.Mirror = m

      override def tpe: scala.reflect.runtime.universe.Type = t

    }

  def of[T](t: Type): TypeTag[T] = {
    new TypeTag[T] {
      // $COVERAGE-OFF$Unused in our context
      override def in[U <: Universe with Singleton](otherMirror: scala.reflect.api.Mirror[U]): U#TypeTag[T] = this.asInstanceOf[U#TypeTag[T]]
      // $COVERAGE-ON$
      override val mirror: universe.Mirror = currentMirror
      override def tpe: universe.Type = t
    }
  }
}