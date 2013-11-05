package co.blocke.scalajack
package fields

import com.fasterxml.jackson.core._

case class OptField( name:String, subField:Field ) extends Field {
	override private[scalajack] def render[T]( sb:StringBuilder, target:T, label:Option[String], ext:Boolean, hint:String, withHint:Boolean=false ) : Boolean = {
		val optVal = target.asInstanceOf[Option[_]]
		if( optVal != None ) {
			// Handle gonzo value types, which are raw datatypes -- EXCEPT when contained in another structure!  Unbox manually here.
			if( subField.isInstanceOf[ValueClassField] ) {
				val valueFieldName = optVal.get.getClass.getDeclaredFields.head.getName
				val unboxedVal = optVal.get.getClass.getDeclaredMethods.toList.find(m => m.getName == valueFieldName).get.invoke(optVal.get)
				subField.render( sb, unboxedVal, label, ext, hint )
			}
			else 
				subField.render( sb, optVal.get, label, ext, hint ) 
			true
		}
		else false 
	}
	override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false ) : Any = {
		val optVal = target.asInstanceOf[Option[_]]
		if( optVal != None ) subField.renderDB( optVal.get, label, hint )
		else optVal
	}
	override private[scalajack] def readValue[T]( jp:JsonParser, ext:Boolean, hint:String )(implicit m:Manifest[T]) : Any = {
		if( subField.isInstanceOf[ValueClassField] ) {
			val sf = subField.asInstanceOf[ValueClassField]
			val raw = sf.readValue(jp,ext,hint)
			Some(sf.constructor.newInstance(raw.asInstanceOf[Object]))
		} else
			Some(subField.readValue(jp,ext,hint))
	}
	override private[scalajack] def readValueDB[T]( src:Any, hint:String )(implicit m:Manifest[T]) : Any = Some(subField.readValueDB(src,hint))
}
