package co.blocke.scalajack

import com.fasterxml.jackson.core._

trait ExtJson[T] extends Any {
	def toJson( obj:T ) : String
	def fromJson( valueType:Field, jp:JsonParser, hint:String, ext:Boolean )(implicit m:Manifest[T]) : Any = valueType.readValue(jp,hint,ext)
}