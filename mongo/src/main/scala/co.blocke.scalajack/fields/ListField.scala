package co.blocke.scalajack
package fields

import com.fasterxml.jackson.core._
import com.mongodb.casbah.Imports._
import scala.collection.JavaConversions._
import scala.language.implicitConversions

import mongo._

object MongoList {

	implicit def BasicDBList2MongoDBList(bdbl : BasicDBList) = new MongoDBList(bdbl)

	implicit class MongoListField( f:ListField ) extends MongoField {
		override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false )(implicit m:Manifest[T]) : Any = {
			val listVal = target.asInstanceOf[Iterable[_]]
			val items = listVal.collect{ case item if(item != None) => f.subField.renderDB(item, None, hint ) }.toArray
			MongoDBList( items: _* )
		}
		override private[scalajack] def readValueDB[T]( src:Any, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = {
			// Dumb down to BasicDBList to avoid double-creation/wrapping of value from database
			val resolved = (src match {
				case mdbl:MongoDBList => mdbl.underlying
				case bdbl:BasicDBList => bdbl
			}).iterator
			val resList = scala.collection.mutable.ListBuffer.empty[Any]
			while( resolved.hasNext ) {
				val item = resolved.next
				resList += f.subField.readValueDB(item,hint,cc)
			}
			resList.toList
		}
	}
}
