package co.blocke.scalajack
package mongo
package typeadapter

import model._

import scala.collection.mutable
import org.bson.types.ObjectId
import org.bson._
import co.blocke.scala_reflection.RType

object ObjectIdTypeAdapter extends TypeAdapterFactory with TypeAdapter[ObjectId]:

  def matches(concrete: RType): Boolean = concrete == info

  def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[_] = this

  val info = RType.of[ObjectId]

  def read(parser: Parser): ObjectId = 
    parser.asInstanceOf[BsonParser].expectObjectId()

  def write[WIRE](t: ObjectId, writer: Writer[WIRE], out: mutable.Builder[WIRE, WIRE]): Unit =
    t match {
      case null => out += new BsonNull().asInstanceOf[WIRE]
      case _ =>
        out += (new BsonObjectId(t)).asInstanceOf[WIRE]
    }