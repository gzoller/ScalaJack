package co.blocke.scalajack
package typeadapter
package collection

import model._
import co.blocke.scala_reflection._

import scala.collection.mutable
import java.lang.reflect.Method


case class MapLikeTypeAdapter[KEY, VALUE, TO <: Map[KEY, VALUE]](
    info:                 RType,
    keyIsOptionalOrAny:   Boolean,
    valueIsOptionalOrAny: Boolean,
    keyTypeAdapter:       TypeAdapter[KEY],
    valueTypeAdapter:     TypeAdapter[VALUE],
    companionInstance:    Object,
    builderMethod:        Method
  ) extends TypeAdapter[TO]:

  def read(parser: Parser): TO =
    // We have to do some voodoo here and peek ahead for Null.  Some types, e.g. Int, aren't nullable,
    // but Maps are nullable, so we can't trust the valueTypeAdapter to catch and handle null in
    // these cases.
    parser.peekForNull match {
      case true               => null.asInstanceOf[TO]
      case _                  => 
        val builder = builderMethod.invoke(companionInstance).asInstanceOf[mutable.Builder[(KEY, VALUE),TO]]
        parser.expectMap(
          keyTypeAdapter,
          valueTypeAdapter,
          builder)
        builder.result
    }

  def write[WIRE](
      t:      TO,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit =
    t match {
      case null => writer.writeNull(out)
      case _    =>
        val filterKey = 
          if (keyIsOptionalOrAny && t != null)
            t.asInstanceOf[Map[KEY, VALUE]].filterNot { case (k, v) => k match {
              case None => true
              case k: java.util.Optional[_] if !k.isPresent => true
              case _ => false
            }}
          else
            t

        val filterValue = 
          if (valueIsOptionalOrAny && t != null)
            filterKey.asInstanceOf[Map[KEY, VALUE]].filterNot { case (k, v) => v match {
              case None => true
              case k: java.util.Optional[_] if !k.isPresent => true
              case _ => false
            }}
          else
            filterKey

        writer.writeMap(
          filterValue.asInstanceOf[Map[KEY, VALUE]],
          keyTypeAdapter,
          valueTypeAdapter,
          out
        )
    }
