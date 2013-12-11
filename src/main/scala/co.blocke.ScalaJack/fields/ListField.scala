package co.blocke.scalajack
package fields

import com.fasterxml.jackson.core._
import com.mongodb.casbah.Imports._
import scala.collection.JavaConversions._
import scala.language.implicitConversions

case class ListField( name:String, subField:Field ) extends Field {
	override private[scalajack] def render[T]( sb:StringBuilder, target:T, label:Option[String], ext:Boolean, hint:String, withHint:Boolean=false )(implicit m:Manifest[T]) : Boolean = {
		val listVal = target.asInstanceOf[Iterable[_]]
		if( listVal.isEmpty ) label.fold( sb.append("[]"))((labelStr) => {
				sb.append('"')
				sb.append(labelStr)
				sb.append("\":[],")
			})
		else {
			val sb2 = new StringBuilder
			listVal.map( item => {
				if( subField.render( sb2, item, None, ext, hint ) ) sb2.append(',') 
			})
			if( sb2.charAt(sb2.length-1) == ',' )
				sb2.deleteCharAt(sb2.length-1)
			label.fold({
					sb.append('[')
					sb.append(sb2)
					sb.append(']')
				})((labelStr) => {
					sb.append('"')
					sb.append(labelStr)
					sb.append("\":[")
					sb.append(sb2)
					sb.append("],")
				})
		}
		true
	}

	implicit def BasicDBList2MongoDBList(bdbl : BasicDBList) = new MongoDBList(bdbl)

	override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false )(implicit m:Manifest[T]) : Any = {
		val listVal = target.asInstanceOf[Iterable[_]]
		val items = listVal.collect{ case item if(item != None) => subField.renderDB(item, None, hint ) }.toArray
		MongoDBList( items: _* )
	}
	override private[scalajack] def readValue[T]( jp:JsonParser, ext:Boolean, hint:String )(implicit m:Manifest[T]) : Any = {
		// Token now sitting on '[' so advance and read list
		jp.nextToken
		val fieldData = scala.collection.mutable.ListBuffer[Any]()
		while( jp.getCurrentToken != JsonToken.END_ARRAY ) {
			fieldData += subField.readValue(jp, ext, hint)
		}
		jp.nextToken
		fieldData.toList
	}
	override private[scalajack] def readValueDB[T]( src:Any, hint:String )(implicit m:Manifest[T]) : Any = {
		// Dumb down to BasicDBList to avoid double-creation/wrapping of value from database
		val resolved = (src match {
			case mdbl:MongoDBList => mdbl.underlying
			case bdbl:BasicDBList => bdbl
		}).iterator
		val resList = scala.collection.mutable.ListBuffer.empty[Any]
		while( resolved.hasNext ) {
			val item = resolved.next
			resList += subField.readValueDB(item,hint)
		}
		resList.toList
	}
}
