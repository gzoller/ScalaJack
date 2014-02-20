package co.blocke.scalajack
package fields

import com.fasterxml.jackson.core._
import com.mongodb.casbah.Imports._

case class TraitProto( typeArgs:List[String] ) extends Field {
	val name = ""
}

case class TraitProxy( name:String, proto:TraitProto ) extends Field 

case class TraitField( name:String, typeArgs:List[String] = List[String]() ) extends Field with ClassOrTrait {
	override private[scalajack] def render[T]( sb:StringBuilder, target:T, label:Option[String], ext:Boolean, hint:String, withHint:Boolean=false )(implicit m:Manifest[T]) : Boolean = {
		Analyzer(target.getClass.getName, typeArgs).render( sb, target, label, ext, hint, true )
	}

	override private[scalajack] def renderClassDB[T]( target:T, hint:String, withHint:Boolean = false )(implicit m:Manifest[T]) : Any = {
		Analyzer(target.getClass.getName, typeArgs).asInstanceOf[CaseClassField].renderClassDB( target, hint, true )
	}

	override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false )(implicit m:Manifest[T]) : Any = {
		Analyzer(target.getClass.getName, typeArgs).renderDB( target, label, hint, true )
	}

	override private[scalajack] def readValue[T]( jp:JsonParser, ext:Boolean, hint:String )(implicit m:Manifest[T]) : Any = readClass(jp,ext,hint)

	override private[scalajack] def readClass[T]( jp:JsonParser, ext:Boolean, hint:String, fromTrait:Boolean = false )(implicit m:Manifest[T]) : Any = {
		if( jp.getCurrentToken != JsonToken.START_OBJECT) throw new IllegalArgumentException("Expected '{'")
		jp.nextToken  // consume '{'
		val fieldData = scala.collection.mutable.Map[String,Any]()
		// read hint
		jp.nextToken // skip _hint label
		val traitClassName = jp.getValueAsString
		Analyzer(traitClassName, typeArgs).asInstanceOf[CaseClassField].readClass( jp, ext, hint, true )
	}

	override private[scalajack] def readClassDB[T]( src:DBObject, hint:String )(implicit m:Manifest[T]) : Any = {
		Analyzer( src.get( hint ).asInstanceOf[String], typeArgs ).asInstanceOf[CaseClassField].readClassDB( src, hint )
	}

	override private[scalajack] def readValueDB[T]( src:Any, hint:String )(implicit m:Manifest[T]) : Any = {
		Analyzer( src.asInstanceOf[DBObject].get( hint ).asInstanceOf[String], typeArgs ).asInstanceOf[CaseClassField].readClassDB( src.asInstanceOf[DBObject], hint )
	}
}
