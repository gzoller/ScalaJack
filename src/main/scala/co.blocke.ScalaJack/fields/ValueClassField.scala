package co.blocke.scalajack
package fields

import com.fasterxml.jackson.core._
import scala.language.existentials // compiler-recommended include

trait _ValueClassField extends Field {

	val name      : String
	val valueType : Field
	val extJson   : Option[ExtJson[_]]

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
		} else {
			println("ValueType: "+valueType)
			valueType.render( sb, target, label, ext, hint )
		}
	}

	override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false ) : Any = 
		valueType.renderDB( target, label, hint )

	override private[scalajack] def readValue[T]( jp:JsonParser, ext:Boolean, hint:String )(implicit m:Manifest[T]) : Any = 
		if( ext && extJson.isDefined )
			extJson.get.fromJson(valueType, jp, ext, hint)
		else
			valueType.readValue(jp,ext,hint)

	override private[scalajack] def readValueDB[T]( src:Any, hint:String )(implicit m:Manifest[T]) : Any = valueType.readValueDB(src,hint)
}

case class ValueClassField( name:String, override val hasMongoAnno:Boolean, valueType:Field, extJson:Option[ExtJson[_]], constructor:java.lang.reflect.Constructor[_] ) extends _ValueClassField

case class ValueClassField2( name:String, override val hasMongoAnno:Boolean, valueType:Field, extJson:Option[ExtJson[_]], constructor:java.lang.reflect.Constructor[_] ) extends _ValueClassField {
	// This is rough!  The target value could be _either_ a value class or a base type, because Scala stores
	// value class objects in a container (Option, List, etc.) and raw base types when a "naked" value, like a field in a
	// case class.  Horrors!
	override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false ) : Any = { 
		val valueFieldName = target.getClass.getDeclaredFields.head.getName
		val value = target.getClass.getDeclaredMethods.toList.find(m => m.getName == valueFieldName).get.invoke(target)
		valueType.renderDB( value, label, hint )
	}
}
