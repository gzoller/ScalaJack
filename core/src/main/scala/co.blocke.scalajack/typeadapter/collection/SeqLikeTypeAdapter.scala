package co.blocke.scalajack
package typeadapter
package collection

import model._
import co.blocke.scala_reflection._

import scala.collection.mutable
import java.lang.reflect.Method


case class SeqLikeTypeAdapter[ELEM, TO](
    info:               RType,
    elemIsOptional:     Boolean,
    elementTypeAdapter: TypeAdapter[ELEM],
    companionInstance:  Object,
    builderMethod:      Method
  ) extends TypeAdapter[TO]:

  def read(parser: Parser): TO =
    // We have to do some voodoo here and peek ahead for Null.  Some types, e.g. Int, aren't nullable,
    // but Seqs are nullable, so we can't trust the valueTypeAdapter to catch and handle null in
    // these cases.
    parser.peekForNull match {
      case true               => null.asInstanceOf[TO]
      case _                  => 
        val builder = builderMethod.invoke(companionInstance).asInstanceOf[mutable.Builder[ELEM,TO]]
        parser.expectList(
          elementTypeAdapter,
          builder
        )
        builder.result
    }

  def write[WIRE](
      t:      TO,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit =
    t match {
      case null                => writer.writeNull(out)
      case _ if elemIsOptional =>
        writer.writeArray(
          t.asInstanceOf[Iterable[ELEM]].filterNot{ case v => v match {
            case None => true
            case v: java.util.Optional[_] if !v.isPresent => true
            case _ => false
          }},
          elementTypeAdapter,
          out
        )
      case _ =>
        writer.writeArray(t.asInstanceOf[Iterable[ELEM]], elementTypeAdapter, out)
    }
