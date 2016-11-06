package co.blocke.scalajack

import org.mongodb.scala.bson.BsonObjectId

import scala.language.implicitConversions

/*
NOTE: None of this doesn't seem to be used anywhere and was throwing off my test numbers... Removed for now.

package object mongo {
  implicit def mongoOID(s: String) = BsonObjectId(s)

  val OBJECT_ID = "org.bson.BsonObjectId"
}
*/

class ObjectId(val bsonObjectId: BsonObjectId) extends AnyVal

/*
Unsure what this is, or why it's commented out... Doesn't appear to be needed.

object ObjectId extends ValueClassCustom {
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