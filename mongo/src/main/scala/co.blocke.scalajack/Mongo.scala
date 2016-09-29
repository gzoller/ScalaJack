package co.blocke.scalajack

import org.mongodb.scala.bson.BsonObjectId

import scala.language.implicitConversions

package object mongo {
  implicit def mongoOID(s: String) = BsonObjectId(s)

  val OBJECT_ID = "org.bson.BsonObjectId"
}

class ObjectId(val bsonObjectId: BsonObjectId) extends AnyVal
/*object ObjectId extends ValueClassCustom {
	def read:PartialFunction[(KindMarker,_), Any] = {
	  case (jk:JsonKind,js:String) => BsonObjectId(js)
	  case (mk:MongoKind,boid:BsonObjectId) => boid
	}
	def render:PartialFunction[(KindMarker,_), Any] = {
	  case (jk:JsonKind,boid:BsonObjectId) => '"'+boid.getValue.toString+'"'
	  case (mk:MongoKind,boid:BsonObjectId) => boid
	}
}
*/ 