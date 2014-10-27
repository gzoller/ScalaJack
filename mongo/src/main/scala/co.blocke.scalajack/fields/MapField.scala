package co.blocke.scalajack
package fields

import com.fasterxml.jackson.core._
import com.mongodb.casbah.Imports._
import scala.collection.JavaConversions._

import mongo._

object MongoMap {

	implicit class MongoMapField( f:MapField ) extends MongoField {
		override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false )(implicit m:Manifest[T]) : Any = {
			val mapVal = target.asInstanceOf[Map[_,_]]
			val mo = MongoDBObject()
			mapVal.collect {  case (k,v) if( v != None ) => {
				mo.put(k.toString, f.valueField.renderDB(v, None, hint ))
				}}
			mo
		}
		override private[scalajack] def readValueDB[T]( src:Any, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = {
			val dbo = src.asInstanceOf[DBObject]
			dbo.keySet.map( key => (key,f.valueField.readValueDB(dbo.get(key), hint, cc)) ).toMap
		}
	}
}
