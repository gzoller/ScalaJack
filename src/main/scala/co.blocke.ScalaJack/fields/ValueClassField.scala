package co.blocke.scalajack
package fields

import com.fasterxml.jackson.core._
import scala.language.existentials // compiler-recommended include

case class ValueClassField( name:String, override val hasMongoAnno:Boolean, valueType:Field, extJson:Option[ExtJson[_]], constructor:java.lang.reflect.Constructor[_] ) extends Field {
	override private[scalajack] def render[T]( sb:StringBuilder, target:T, label:Option[String], ext:Boolean, hint:String, withHint:Boolean=false ) : Boolean = {
		if( ext && extJson.isDefined ) {
			label.fold( {
					sb.append( extJson.get.asInstanceOf[ExtJson[T]].toJson( target ) )
				})((labelStr) => {
					sb.append('"')
					sb.append( labelStr )
					sb.append("\":")
					sb.append( extJson.get.asInstanceOf[ExtJson[T]].toJson( target ) )
					sb.append(',')
				})			
			true
		} else 
			valueType.render( sb, target, label, ext, hint )
	}
	override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false ) : Any = { 
		target
	}
	override private[scalajack] def readValue[T]( jp:JsonParser, ext:Boolean, hint:String )(implicit m:Manifest[T]) : Any = {
		if( ext && extJson.isDefined )
			extJson.get.fromJson(valueType, jp, ext, hint)
		else
			valueType.readValue(jp,ext,hint)
	}
	override private[scalajack] def readValueDB[T]( src:Any, hint:String )(implicit m:Manifest[T]) : Any = valueType.readValueDB(src,hint)
}
