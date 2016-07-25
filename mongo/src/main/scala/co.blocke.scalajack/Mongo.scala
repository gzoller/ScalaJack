package co.blocke.scalajack

import json.JsonKind
import scala.collection.mutable.LinkedHashMap
import scala.language.implicitConversions
import org.mongodb.scala._
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.bson.BsonObjectId

package object mongo {
	implicit def mongoOID( s:String ) = BsonObjectId(s)

	val OBJECT_ID = "org.bson.BsonObjectId"
}
import mongo._

class ObjectId( val bsonObjectId:BsonObjectId ) extends AnyVal
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
