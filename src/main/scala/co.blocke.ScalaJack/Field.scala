package co.blocke.scalajack

import com.fasterxml.jackson.core._
import com.mongodb.casbah.Imports._

trait Field {
	private[scalajack] val name         : String
	private[scalajack] val hasMongoAnno : Boolean = false

	private[scalajack] def render[T]( sb:StringBuilder, target:T, label:Option[String], ext:Boolean, hint:String, withHint:Boolean=false ) : Boolean = {
		label.fold( {
				sb.append(target)
			})((labelStr) => {
				sb.append('"')
				sb.append( labelStr )
				sb.append("\":")
				sb.append(target)
				sb.append(',')
			})
		true
	}
	private[scalajack] def readValue[T]( jp:JsonParser, ext:Boolean, hint:String )(implicit m:Manifest[T]) : Any = { 0 }

	private[scalajack] def renderDB[T]( target:T, label:Option[String], hint:String, withHint:Boolean = false ) : Any = { 0 }
	private[scalajack] def readValueDB[T]( src:Any, hint:String )(implicit m:Manifest[T]) : Any = { 0 }
}

trait ClassOrTrait {
	private[scalajack] def readClass[T]( jp:JsonParser, ext:Boolean, hint:String )(implicit m:Manifest[T]) : Any

	private[scalajack] def renderClassDB[T]( target:T, hint:String, withHint:Boolean = false ) : Any
	private[scalajack] def readClassDB[T]( src:DBObject, hint:String )(implicit m:Manifest[T]) : Any
}