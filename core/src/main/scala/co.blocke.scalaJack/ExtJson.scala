package co.blocke.scalajack

/**
 * Mix this trait into a value class' companion object to enable an optional ability for extended
 * JSON rendering/reading.  If you have this trait mixed you can toggle the extended rendering 
 * and reading with the optional 'ext' (Boolean) field in ScalaJack's read and render functions.
 *
 * Note we must account for both unboxed and value class instances due to how Scala may or may not
 * actually instantiate a value class.
 */
trait ExtJson extends Any {
	def toJson( obj:Any ) : String
	def fromJson( valueType:Field, jp:JsonEmitter, ext:Boolean, hint:String ) : Any
}