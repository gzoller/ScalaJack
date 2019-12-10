package co.blocke.scalajack
package mongo

import model._

import scala.collection.mutable
import org.bson.types.ObjectId
import org.bson._

object ObjectIdTypeAdapter extends TypeAdapter.===[ObjectId] {

  def read(parser: Parser): ObjectId =
    parser.asInstanceOf[BsonParser].expectObjectId()

  def write[WIRE](
      t:      ObjectId,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit =
    t match {
      case null => out += new BsonNull().asInstanceOf[WIRE]
      case _ =>
        out += (new BsonObjectId(t)).asInstanceOf[WIRE]
    }
}
