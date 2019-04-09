package co.blocke.scalajack
package mongo

import util.Path
import model._

import scala.collection.mutable.Builder
import org.bson._
import java.time._

object OffsetDateTimeTypeAdapterFactory extends TypeAdapter.=:=[OffsetDateTime] {

  def read[WIRE](path: Path, reader: Reader[WIRE]): OffsetDateTime =
    reader.head.input match {
      case null =>
        reader.next
        null
      // $COVERAGE-OFF$Doesn't seem to be reachable -- naked nulls passed from Bson, but just in case...
      case i if i.asInstanceOf[BsonValue].isNull() =>
        reader.next
        null
      // $COVERAGE-ON$
      case _ =>
        val dateTimeLong = reader.asInstanceOf[MongoReader].readDateTime(path)
        OffsetDateTime.ofInstant(Instant.ofEpochMilli(dateTimeLong), ZoneOffset.UTC)
    }

  def write[WIRE](t: OffsetDateTime, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit =
    t match {
      case null => out += new BsonNull().asInstanceOf[WIRE]
      case _ =>
        out += new BsonDateTime(t.toInstant.toEpochMilli).asInstanceOf[WIRE]
    }
}
