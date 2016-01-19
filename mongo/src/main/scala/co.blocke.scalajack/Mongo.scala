package co.blocke.scalajack

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

// Custom type
case class ObjectIdType(name:String) extends CustomType {
	val renderers = Map(  // ObjectId -> <something>
		"default" -> ((a:Any) => "\""+a.asInstanceOf[BsonObjectId].getValue.toString+"\""),
		"mongo"   -> ((a:Any) => a)
		)
	val readers   = Map(
		"default" -> ((a:Any) => BsonObjectId(a.toString)),
		"mongo"   -> ((a:Any) => a)
		)
	def dup = this.copy()
}
