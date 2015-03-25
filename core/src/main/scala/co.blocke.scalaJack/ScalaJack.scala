package co.blocke.scalajack

/**
 * Assumptions:
 *   -- Case classes only
 *   -- Options of value None are removed from generated output
 *   -- Default parameters are not supported at this time
 *   -- MongoKey-decorated fields must be type String and map to "_id" (Mongo's default)
 *   
 * Useful references:
 * 		http://stackoverflow.com/questions/17006271/deep-access-of-fields-in-scala-using-runtime-reflection
 */

import scala.reflect.runtime.universe._
import json._

case class ValClassHandler(
	read:(String) => Any,
	render:(Any)  => String
	)

case class VisitorContext(
	typeHint       : String  = "_hint",
	isCanonical    : Boolean = true,    // allow non-string keys in Maps--not part of JSON spec
	isValidating   : Boolean = false,
	estFieldsInObj : Int     = 128,
	valClassMap    : Map[String,ValClassHandler] = Map.empty[String,ValClassHandler]
	// hintMap : Map[String,String] = Map.empty[String,String]
	)


case class ScalaJack_JSON() extends ScalaJack[String] with JSONReadRenderFrame 

trait SupportedType[U] {
	def makeScalaJack():ScalaJack[U]
}
case class JsonType() extends SupportedType[String] {
	def makeScalaJack():ScalaJack[String] = ScalaJack_JSON()
}
  
object ScalaJack {
	def apply[R]( kind:SupportedType[R] = JsonType() ) : ScalaJack[R] = kind.makeScalaJack

	// Legacy support (JSON implied)
	private val jsonJS = apply()
	def read[T](js:String, hint:String="_hint")(implicit tt:TypeTag[T]) = jsonJS.read(js,VisitorContext(hint,true,true))
	def render[T](instance:T, hint:String="_hint")(implicit tt:TypeTag[T]) = jsonJS.render(instance,VisitorContext(hint,true,true)).toString
}

trait ScalaJack[R] {
	this: ReadRenderFrame[R] =>
	def read[T](in:R, vctx:VisitorContext=VisitorContext())(implicit tt:TypeTag[T]) = {
		implicit val vc = vctx
		renderer.read(in)
	}
	def render[T](instance:T, vctx:VisitorContext=VisitorContext())(implicit tt:TypeTag[T]) = {
		implicit val vc = vctx
		renderer.render(instance)
	}
}
