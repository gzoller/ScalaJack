package co.blocke.scalajack
package fields

import com.fasterxml.jackson.core._

case class TraitProto( typeArgs:List[String] ) extends Field {
	val name = ""
}

case class TraitProxy( name:String, proto:TraitProto ) extends Field 

case class TraitField( name:String, typeArgs:List[String] = List[String]() ) extends Field with ClassOrTrait {
	override private[scalajack] def render[T]( sb:StringBuilder, target:T, label:Option[String], ext:Boolean, hint:String, withHint:Boolean=false )(implicit m:Manifest[T]) : Boolean = {
		Analyzer.inspect(target.getClass.getName, typeArgs).render( sb, target, label, ext, hint, true )
	}

	override private[scalajack] def readValue[T]( jp:JsonEmitter, ext:Boolean, hint:String, cc:ClassContext )(implicit m:Manifest[T]) : Any = readClass(jp,ext,hint)

	override private[scalajack] def readClass[T]( jp:JsonEmitter, ext:Boolean, hint:String, fromTrait:Boolean = false )(implicit m:Manifest[T]) : Any = {
		if( jp.getCurrentToken != JsonToken.START_OBJECT) throw new IllegalArgumentException("Expected '{'")
		jp.nextToken  // consume '{'
		// peek-ahead to read hint
		var hintClass = ""
		while( hintClass.length == 0 && jp.getCurrentToken != null ) {
			val t = jp.getCurrentToken
			if( t == JsonToken.END_OBJECT )
				throw new IllegalArgumentException("Didn't find hint field "+hint+" in data for trait.")
			if( jp.getCurrentName == hint ) {
				jp.peekNextToken
				hintClass = jp.getValueAsString
				jp.restoreState
			}
			else{
				 jp.peekNextToken // skip to value
				 jp.skipChildren(true) // skip value and any kids -- skip to next field name
				}
		}
		Analyzer.inspect(hintClass, typeArgs).asInstanceOf[CaseClassField].readClass( jp, ext, hint, true )
	}
}
