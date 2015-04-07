package co.blocke.scalajack

import scala.collection.mutable.LinkedHashMap
import scala.language.implicitConversions
import com.mongodb.casbah.Imports._

package object mongo {
	implicit def mongoOID( s:String ) = new ObjectId( s)

	val OBJECT_ID = "org.bson.types.ObjectId"
}
import mongo._

// Custom type
case class ObjectIdType(name:String) extends CustomType {
	val renderers = Map(  // ObjectId -> <something>
		"default" -> ((a:Any) => "\""+a+"\""),
		"mongo"   -> ((a:Any) => a)
		)
	val readers   = Map(
		"default" -> ((a:Any) => new ObjectId(a.toString)),
		"mongo"   -> ((a:Any) => a)
		)
	def dup = this.copy()
}

case class ScalaJack_Mongo() extends ScalaJack[MongoDBObject] with mongo.MongoReadRenderFrame {
	Analyzer.addType(OBJECT_ID,ObjectIdType(OBJECT_ID))
} 

case class MongoType() extends SupportedType[MongoDBObject] {
	def makeScalaJack():ScalaJack[MongoDBObject] = ScalaJack_Mongo()
}
