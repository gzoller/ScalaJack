package co.blocke.scalajack
package fields

case class OptField( name:String, subField:Field, override val hasDBKeyAnno:Boolean = false ) extends Field {
	override private[scalajack] def render[T]( sb:StringBuilder, target:T, label:Option[String], ext:Boolean, hint:String, withHint:Boolean=false )(implicit m:Manifest[T]) : Boolean = {
		val optVal = target.asInstanceOf[Option[_]]
		if( optVal != None ) {
			subField.render( sb, optVal.get, label, ext, hint ) 
			true
		}
		else false 
	}
	override private[scalajack] def readValue[T]( jp:JsonEmitter, ext:Boolean, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = {
		Some(subField.readValue(jp,ext,hint,cc))
	}
}
