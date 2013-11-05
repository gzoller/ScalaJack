package co.blocke.scalajack
package fields

import com.fasterxml.jackson.core._
import com.mongodb.casbah.Imports._
import scala.collection.JavaConversions._

case class ListField( name:String, subField:Field ) extends Field {
	override private[scalajack] def render[T]( sb:StringBuilder, target:T, label:Option[String], ext:Boolean, hint:String, withHint:Boolean=false ) : Boolean = {
		val listVal = target.asInstanceOf[Iterable[_]]
		if( listVal.isEmpty ) label.fold( sb.append("[]"))((labelStr) => {
				sb.append('"')
				sb.append(labelStr)
				sb.append("\":[],")
			})
		else {
			val sb2 = new StringBuilder
			if( subField.isInstanceOf[ValueClassField] ) {
				listVal.map( item => {
					val valueFieldName = item.getClass.getDeclaredFields.head.getName
					val unboxedVal = item.getClass.getDeclaredMethods.toList.find(m => m.getName == valueFieldName).get.invoke(item)
					if( subField.render( sb2, unboxedVal, None, ext, hint ) ) sb2.append(',') 
				})
			} else {
				listVal.map( item => {
					if( subField.render( sb2, item, None, ext, hint ) ) sb2.append(',') 
				})
			}
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
	override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false ) : Any = {
		val listVal = target.asInstanceOf[Iterable[_]]
		if( subField.isInstanceOf[ValueClassField] ) {
			val items = listVal.collect{ case item => {
				val valueFieldName = item.getClass.getDeclaredFields.head.getName
				val unboxedVal = item.getClass.getDeclaredMethods.toList.find(m => m.getName == valueFieldName).get.invoke(item)
				subField.renderDB(unboxedVal, None, hint )
			} }.toArray
			MongoDBList( items: _* )
		} else {
			val items = listVal.collect{ case item if(item != None) => subField.renderDB(item, None, hint ) }.toArray
			MongoDBList( items: _* )
		}
	}
	override private[scalajack] def readValue[T]( jp:JsonParser, ext:Boolean, hint:String )(implicit m:Manifest[T]) : Any = {
		// Token now sitting on '[' so advance and read list
		jp.nextToken
		val fieldData = scala.collection.mutable.ListBuffer[Any]()
		if( subField.isInstanceOf[ValueClassField] ) {
			while( jp.getCurrentToken != JsonToken.END_ARRAY ) {
				val sf = subField.asInstanceOf[ValueClassField]
				val raw = sf.readValue(jp,ext,hint)
				fieldData += sf.constructor.newInstance(raw.asInstanceOf[Object])
			}
		} else {
			while( jp.getCurrentToken != JsonToken.END_ARRAY ) {
				fieldData += subField.readValue(jp, ext, hint)
			}
		}
		jp.nextToken
		fieldData.toList
	}
	override private[scalajack] def readValueDB[T]( src:Any, hint:String )(implicit m:Manifest[T]) : Any = {
		if( subField.isInstanceOf[ValueClassField] ) {
			val sf = subField.asInstanceOf[ValueClassField]
			src.asInstanceOf[MongoDBList].map( item => {
				val raw = sf.readValueDB(item, hint)
				sf.constructor.newInstance(raw.asInstanceOf[Object])
			}).toList
		} else
			src.asInstanceOf[MongoDBList].map( item => subField.readValueDB(item, hint)).toList
	}
}
