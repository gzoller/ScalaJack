package co.blocke.scalajack
package fields

import org.bson.types.ObjectId
import com.fasterxml.jackson.core._

case class ObjectIdField( name:String ) extends Field with MongoField {

	override private[scalajack] def render[T]( sb:StringBuilder, target:T, label:Option[String], ext:Boolean, hint:String, withHint:Boolean=false )(implicit m:Manifest[T]) : Boolean = {
		label.fold( {
				sb.append('"')
				sb.append( target.toString )
				sb.append('"')
			})((labelStr) => {
				sb.append('"')
				sb.append( labelStr )
				sb.append("\":\"")
				sb.append( target.toString )
				sb.append("\",")
			})
		true
	}

	override private[scalajack] def readValue[T]( jp:JsonEmitter, ext:Boolean, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = {
		if( jp.getCurrentToken != JsonToken.VALUE_STRING && jp.getCurrentToken != JsonToken.VALUE_NULL ) throw new IllegalArgumentException("Class "+cc.className+" field "+cc.fieldName+" Expected VALUE_STRING and saw "+jp.getCurrentToken)
		val v = jp.getValueAsString
		jp.nextToken
		new ObjectId(v)
	}

	override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false )(implicit m:Manifest[T]) : Any = 
		target.asInstanceOf[ObjectId]

	override private[scalajack] def readValueDB[T]( src:Any, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = src
}
