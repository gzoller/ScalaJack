package co.blocke.scalajack
package mongo
package typeadapter

import scala.collection.mutable
import org.bson._
import java.time._
import co.blocke.scala_reflection.RType

import model._

object ZonedDateTimeTypeAdapter extends TypeAdapterFactory with TypeAdapter[ZonedDateTime]:

  def matches(concrete: RType): Boolean = concrete == info

  def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[_] = this

  val info = RType.of[ZonedDateTime]

  def read(parser: Parser): ZonedDateTime = 
    parser.expectNumber(true) match {
      case null => null
      case dateTimeLong =>
        ZonedDateTime.ofInstant(
          Instant.ofEpochMilli(dateTimeLong.toLong),
          ZoneId.of("UTC")
        )
    }

  def write[WIRE](t: ZonedDateTime, writer: Writer[WIRE], out: mutable.Builder[WIRE, WIRE]): Unit =
    t match {
      case null =>
        out += new BsonNull().asInstanceOf[WIRE]
      case _ =>
        out += new BsonDateTime(
          t.withZoneSameInstant(ZoneId.of("UTC")).toInstant.toEpochMilli
        ).asInstanceOf[WIRE]
    }
