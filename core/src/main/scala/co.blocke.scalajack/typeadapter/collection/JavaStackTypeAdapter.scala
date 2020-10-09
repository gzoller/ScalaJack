package co.blocke.scalajack
package typeadapter
package collection

import model._
import co.blocke.scala_reflection._

import scala.collection.mutable
import java.lang.reflect.{Method, Constructor}
import scala.jdk.CollectionConverters._


case class JavaStackTypeAdapter[ELEM, TO](
    info:                RType,
    elemIsOptional:      Boolean,
    elementTypeAdapter:  TypeAdapter[ELEM],
    companionInstance:   Object,
    builderMethod:       Method,
    javaCollConstructor: Constructor[_],
    toArrayMethod:       Method
  ) extends TypeAdapter[TO]:

  def read(parser: Parser): TO =
    // We have to do some voodoo here and peek ahead for Null.  Some types, e.g. Int, aren't nullable,
    // but Seqs are nullable, so we can't trust the valueTypeAdapter to catch and handle null in
    // these cases.
    parser.peekForNull match {
      case true               => null.asInstanceOf[TO]
      case _                  => 
        val builder = builderMethod.invoke(companionInstance).asInstanceOf[mutable.Builder[ELEM,List[ELEM]]]
        parser.expectList(
          elementTypeAdapter,
          builder
        )
        val res: List[ELEM] = builder.result
        val stackInstance = javaCollConstructor.newInstance().asInstanceOf[java.util.Stack[ELEM]]
        res.map( e => stackInstance.push(e) )
        stackInstance.asInstanceOf[TO]
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
        val iterable = toArrayMethod.invoke(t).asInstanceOf[Array[ELEM]].toIterable
        writer.writeArray(iterable, elementTypeAdapter, out)
    }
