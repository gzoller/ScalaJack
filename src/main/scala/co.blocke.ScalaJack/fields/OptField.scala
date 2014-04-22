package co.blocke.scalajack
package fields

import com.fasterxml.jackson.core._

case class OptField( name:String, subField:Field, override val hasMongoAnno:Boolean = false ) extends Field {
	override private[scalajack] def render[T]( sb:StringBuilder, target:T, label:Option[String], ext:Boolean, hint:String, withHint:Boolean=false )(implicit m:Manifest[T]) : Boolean = {
		val optVal = target.asInstanceOf[Option[_]]
		if( optVal != None ) {
			subField.render( sb, optVal.get, label, ext, hint ) 
			true
		}
		else false 
	}
	override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false )(implicit m:Manifest[T]) : Any = {
		val optVal = target.asInstanceOf[Option[_]]
		if( optVal != None ) subField.renderDB( optVal.get, label, hint )
		else optVal
	}
	override private[scalajack] def readValue[T]( jp:JsonParser, ext:Boolean, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = {
		Some(subField.readValue(jp,ext,hint,cc))
	}
	override private[scalajack] def readValueDB[T]( src:Any, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = Some(subField.readValueDB(src,hint,cc))
}
