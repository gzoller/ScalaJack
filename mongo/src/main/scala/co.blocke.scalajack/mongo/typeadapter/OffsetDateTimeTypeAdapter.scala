package co.blocke.scalajack
package mongo

import scala.util.{ Try, Success, Failure }
import model._

import scala.collection.mutable
import org.bson._
import java.time._

object OffsetDateTimeTypeAdapter extends TypeAdapter.=:=[OffsetDateTime] {

  def read(parser: Parser): OffsetDateTime =
    parser.expectNumber() match {
      case null => null
      case dateTimeLong =>
        OffsetDateTime.ofInstant(
          Instant.ofEpochMilli(dateTimeLong.toLong),
          ZoneOffset.UTC
        )
    }

  def write[WIRE](
      t:      OffsetDateTime,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit =
    t match {
      case null => out += new BsonNull().asInstanceOf[WIRE]
      case _ =>
        out += new BsonDateTime(t.toInstant.toEpochMilli).asInstanceOf[WIRE]
    }
}
