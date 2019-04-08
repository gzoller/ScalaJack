package co.blocke.scalajack
package mongo

import util.Path
import model._

import scala.collection.mutable.Builder
import org.mongodb.scala.bson._

object ObjectIdTypeAdapterFactory extends TypeAdapter.===[ObjectId] {

  def read[WIRE](path: Path, reader: Reader[WIRE]): ObjectId =
    reader.head.input match {
      case BsonNull => null
      case _ =>
        reader.asInstanceOf[MongoReader].readObjectId(path)
    }

  def write[WIRE](t: ObjectId, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit =
    t match {
      case null => out += BsonNull().asInstanceOf[WIRE]
      case _ =>
        out += (new BsonObjectId(t)).asInstanceOf[WIRE]
    }
}
