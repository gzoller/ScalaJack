package co.blocke.scalajack
package fields

import com.fasterxml.jackson.core._

case class EnumField( name:String, enum:Enumeration ) extends Field {
	override private[scalajack] def render[T]( sb:StringBuilder, target:T, label:Option[String], ext:Boolean, hint:String, withHint:Boolean=false )(implicit m:Manifest[T]) : Boolean = {
		label.fold( {
				sb.append('"')
				sb.append(target)
				sb.append('"')
			})((labelStr) => {
				sb.append('"')
				sb.append( labelStr )
				sb.append("\":\"")
				sb.append(target)
				sb.append("\",")
			})
		true
	}
	override private[scalajack] def readValue[T]( jp:JsonEmitter, ext:Boolean, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = {
		if( jp.getCurrentToken != JsonToken.VALUE_STRING) throw new IllegalArgumentException("Class "+cc.className+" field "+cc.fieldName+" Expected VALUE_STRING (enum) and saw "+jp.getCurrentToken)
		val v = scala.util.Try( enum.withName(jp.getValueAsString) ).toOption.getOrElse( throw new IllegalArgumentException("Class "+cc.className+" field "+cc.fieldName+" Given value of "+jp.getValueAsString+" is not valid for this enum field." ) )
		jp.nextToken
		v
	}
}
