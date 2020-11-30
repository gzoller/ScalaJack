package co.blocke.scalajack
package mongo
package typeadapter

import model._

import scala.collection.mutable
import org.bson._
import java.time._
import co.blocke.scala_reflection.RType

object OffsetDateTimeTypeAdapter extends TypeAdapterFactory with TypeAdapter[OffsetDateTime]:

  def matches(concrete: RType): Boolean = concrete == info

  def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[_] = this

  val info = RType.of[OffsetDateTime]

  def read(parser: Parser): OffsetDateTime = 
    parser.expectNumber(true) match {
      case null => null
      case dateTimeLong =>
        OffsetDateTime.ofInstant(
          Instant.ofEpochMilli(dateTimeLong.toLong),
          ZoneOffset.UTC
        )
    }

  def write[WIRE](t: OffsetDateTime, writer: Writer[WIRE], out: mutable.Builder[WIRE, WIRE]): Unit =
    t match {
      case null => out += new BsonNull().asInstanceOf[WIRE]
      case _ =>
        out += new BsonDateTime(t.toInstant.toEpochMilli).asInstanceOf[WIRE]
    }
