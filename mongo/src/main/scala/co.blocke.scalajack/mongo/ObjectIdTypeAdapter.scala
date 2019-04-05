package co.blocke.scalajack
package mongo

import util.Path
import model._

import scala.collection.mutable.Builder
import org.mongodb.scala.bson._

object ObjectIdTypeAdapterFactory extends TypeAdapter.=:=[ObjectId] {

  def read[BsonValue](path: Path, reader: Reader[BsonValue]): ObjectId =
    reader.asInstanceOf[MongoReader].readObjectId(path)

  def write[BsonValue](t: ObjectId, writer: Writer[BsonValue], out: Builder[BsonValue, BsonValue], isMapKey: Boolean): Unit =
    out += (new BsonObjectId(t)).asInstanceOf[BsonValue]
}
