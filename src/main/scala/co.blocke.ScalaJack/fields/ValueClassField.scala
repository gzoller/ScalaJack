package co.blocke.scalajack
package fields

import com.fasterxml.jackson.core._
import scala.language.existentials // compiler-recommended include
import java.lang.reflect.Constructor

case class ValueClassFieldUnboxed( name:String, override val hasMongoAnno:Boolean, valueType:Field, extJson:Option[ExtJson] ) extends Field {

	override private[scalajack] def render[T]( sb:StringBuilder, target:T, label:Option[String], ext:Boolean, hint:String, withHint:Boolean=false )(implicit m:Manifest[T]) : Boolean = {
		if( ext && extJson.isDefined ) {
			label.fold( {
					sb.append( extJson.get.asInstanceOf[ExtJson].toJson( target ) )
				})((labelStr) => {
					sb.append('"')
					sb.append( labelStr )
					sb.append("\":")
					sb.append( extJson.get.asInstanceOf[ExtJson].toJson( target ) )
					sb.append(',')
				})			
			true
		} else 
			valueType.render( sb, target, label, ext, hint )
	}

	override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false )(implicit m:Manifest[T]) : Any = 
		valueType.renderDB( target, label, hint )

	override private[scalajack] def readValue[T]( jp:JsonEmitter, ext:Boolean, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = 
		if( ext && extJson.isDefined )
			extJson.get.fromJson(valueType, jp, ext, hint)
		else
			valueType.readValue(jp,ext,hint, cc)

	override private[scalajack] def readValueDB[T]( src:Any, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = valueType.readValueDB(src,hint,cc)
}

//----------------------------------------------------------------------------------

// This is rough!  The target value could be _either_ a value class or a base type, because Scala stores
// value class objects in a container (Option, List, etc.) and raw base types when a "naked" value, like a field in a
// case class.  Horrors!

case class ValueClassField( name:String, override val hasMongoAnno:Boolean, valueType:Field, constructor:Constructor[_], extJson:Option[ExtJson] ) extends Field {

	override private[scalajack] def render[T]( sb:StringBuilder, target:T, label:Option[String], ext:Boolean, hint:String, withHint:Boolean=false )(implicit m:Manifest[T]) : Boolean = {
		val valueFieldName = target.getClass.getDeclaredFields.head.getName
		val unboxedVal = target.getClass.getDeclaredMethods.toList.find(m => m.getName == valueFieldName).get.invoke(target)
		if( ext && extJson.isDefined ) {
			label.fold( {
					sb.append( extJson.get.asInstanceOf[ExtJson].toJson( unboxedVal ) )
				})((labelStr) => {
					sb.append('"')
					sb.append( labelStr )
					sb.append("\":")
					sb.append( extJson.get.asInstanceOf[ExtJson].toJson( unboxedVal ) )
					sb.append(',')
				})			
			true
		} else 
			valueType.render( sb, unboxedVal, label, ext, hint )
	}

	override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false )(implicit m:Manifest[T]) : Any = { 
		val valueFieldName = target.getClass.getDeclaredFields.head.getName
		val value = target.getClass.getDeclaredMethods.toList.find(m => m.getName == valueFieldName).get.invoke(target)
		valueType.renderDB( value, label, hint )
	}

	override private[scalajack] def readValue[T]( jp:JsonEmitter, ext:Boolean, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = {
		val value = { if( ext && extJson.isDefined )
				extJson.get.fromJson(valueType, jp, ext, hint)
			else
				valueType.readValue(jp,ext,hint,cc)
		}
		constructor.newInstance( value.asInstanceOf[Object] )
	}

	override private[scalajack] def readValueDB[T]( src:Any, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = {
		constructor.newInstance( valueType.readValueDB(src,hint,cc).asInstanceOf[Object] )
	}
}
