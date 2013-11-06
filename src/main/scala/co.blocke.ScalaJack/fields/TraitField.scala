package co.blocke.scalajack
package fields

import com.fasterxml.jackson.core._
import com.mongodb.casbah.Imports._

case class TraitField( name:String ) extends Field with ClassOrTrait {
	override private[scalajack] def render[T]( sb:StringBuilder, target:T, label:Option[String], ext:Boolean, hint:String, withHint:Boolean=false ) : Boolean = {
		Analyzer(target.getClass.getName).render( sb, target, label, ext, hint, true )
	}

	override private[scalajack] def renderClassDB[T]( target:T, hint:String, withHint:Boolean = false ) : Any = {
		Analyzer(target.getClass.getName).asInstanceOf[CaseClassField].renderClassDB( target, hint, true )
	}

	override private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false ) : Any = {
		Analyzer(target.getClass.getName).renderDB( target, label, hint, true )
	}

	override private[scalajack] def readValue[T]( jp:JsonParser, ext:Boolean, hint:String )(implicit m:Manifest[T]) : Any = readClass(jp,ext,hint)

	override private[scalajack] def readClass[T]( jp:JsonParser, ext:Boolean, hint:String )(implicit m:Manifest[T]) : Any = {
		jp.nextToken  // consume '{'
		val fieldData = scala.collection.mutable.Map[String,Any]()
		// read hint
		jp.nextToken // skip _hint label
		val traitClassName = jp.getValueAsString
		Analyzer(traitClassName).asInstanceOf[CaseClassField].readClass( jp, ext, hint )
	}

	override private[scalajack] def readClassDB[T]( src:DBObject, hint:String )(implicit m:Manifest[T]) : Any = {
		Analyzer( src.get( hint ).asInstanceOf[String] ).asInstanceOf[CaseClassField].readClassDB( src, hint )
	}

	override private[scalajack] def readValueDB[T]( src:Any, hint:String )(implicit m:Manifest[T]) : Any = {
		Analyzer( src.asInstanceOf[DBObject].get( hint ).asInstanceOf[String] ).asInstanceOf[CaseClassField].readClassDB( src.asInstanceOf[DBObject], hint )
	}
}
