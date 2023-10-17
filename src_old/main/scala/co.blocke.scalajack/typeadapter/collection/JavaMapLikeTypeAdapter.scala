package co.blocke.scalajack
package typeadapter
package collection

import model._
import co.blocke.scala_reflection._

import scala.collection.mutable
import java.lang.reflect.{Method, Constructor}
import scala.jdk.CollectionConverters._


case class JavaMapLikeTypeAdapter[KEY, VALUE, TO <: java.util.Map[KEY, VALUE]](
    info:                 RType,
    keyIsOptionalOrAny:   Boolean,
    valueIsOptionalOrAny: Boolean,
    keyTypeAdapter:       TypeAdapter[KEY],
    valueTypeAdapter:     TypeAdapter[VALUE],
    companionInstance:    Object,
    builderMethod:        Method,
    javaMapConstructor:   Constructor[_],
  ) extends TypeAdapter[TO]:

  def read(parser: Parser): TO = 
    // We have to do some voodoo here and peek ahead for Null.  Some types, e.g. Int, aren't nullable,
    // but Maps are nullable, so we can't trust the valueTypeAdapter to catch and handle null in
    // these cases.
    parser.peekForNull match {
      case true               => null.asInstanceOf[TO]
      case _                  => 
        val builder = builderMethod.invoke(companionInstance).asInstanceOf[mutable.Builder[(KEY, VALUE),Map[KEY,VALUE]]]
        parser.expectMap(
          keyTypeAdapter,
          valueTypeAdapter,
          builder)
        javaMapConstructor.newInstance(builder.result.asJava).asInstanceOf[TO]
    }

  def write[WIRE](
      t:      TO,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit =
    t match {
      case null => writer.writeNull(out)
      case _    =>
        val tScala = t.asScala
        val filterKey = 
          if (keyIsOptionalOrAny && tScala != null)
            tScala.asInstanceOf[Map[KEY, VALUE]].filterNot { case (k, v) => k match {
              case None => true
              case k: java.util.Optional[_] if !k.isPresent => true
              case _ => false
            }}
          else
            tScala

        val filterValue = 
          if valueIsOptionalOrAny && tScala != null then
            filterKey.filterNot { case (k, v) => v match {
              case None => true
              case k: java.util.Optional[_] if !k.isPresent => true
              case _ => false
            }}
          else
            filterKey

        writer.writeMap(
          filterValue,
          keyTypeAdapter,
          valueTypeAdapter,
          out
        )
    }
