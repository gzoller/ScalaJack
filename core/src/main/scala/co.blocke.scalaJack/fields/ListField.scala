package co.blocke.scalajack
package fields

import com.fasterxml.jackson.core._
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

	override private[scalajack] def readValue[T]( jp:JsonEmitter, ext:Boolean, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = {
		// Token now sitting on '[' so advance and read list
		if( jp.getCurrentToken != JsonToken.START_ARRAY) throw new IllegalArgumentException("Class "+cc.className+" field "+cc.fieldName+" Expected '['")
		jp.nextToken
		val fieldData = scala.collection.mutable.ListBuffer[Any]()
		while( jp.getCurrentToken != JsonToken.END_ARRAY ) {
			fieldData += subField.readValue(jp, ext, hint, cc)
		}
		jp.nextToken
		fieldData.toList
	}
}
