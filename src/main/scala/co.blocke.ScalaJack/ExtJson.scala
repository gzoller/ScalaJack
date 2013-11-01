package co.blocke.scalajack

import com.fasterxml.jackson.core._

/**
 * Mix this trait into a value class' companion object to enable an optional ability for extended
 * JSON rendering/reading.  If you have this trait mixed you can toggle the extended rendering 
 * and reading with the optional 'ext' (Boolean) field in ScalaJack's read and render functions.
 */
trait ExtJson[T] extends Any {
	def toJson( obj:T ) : String
	def fromJson( valueType:Field, jp:JsonParser, ext:Boolean, hint:String )(implicit m:Manifest[T]) : Any = valueType.readValue(jp,ext,hint)
}