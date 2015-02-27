package co.blocke.scalajack

/**
 * Assumptions:
 *   -- Case classes only
 *   -- Options of value None are removed from generated output
 *   -- Default parameters are not supported at this time
 *   -- Simple types supported:
 *         Int, Boolean, Long, String, Enumeration.Value
 *   -- Collections/"containers" supported:
 *         List (mutable (ListBuffer) and immutable)
 *         Set  (mutable and immutable)
 *         Map  (mutable and immutable)
 *         Option
 *   -- MongoKey-decorated fields must be type String and map to "_id" (Mongo's default)
 *   
 * Useful references:
 * 		http://stackoverflow.com/questions/17006271/deep-access-of-fields-in-scala-using-runtime-reflection
 */

import scala.reflect.runtime.universe._
import formats._
import json._

object Formats extends Enumeration {
	type Format = Value
	val JSON, XML, Custom = Value
}
import Formats._


case class VisitorContext(
	typeHint       : String  = "_hint",
	isCanonical    : Boolean = true,    // allow non-string keys in Maps--not part of JSON spec
	isValidating   : Boolean = false,
	estFieldsInObj : Int     = 128
	)
  
object ScalaJack {
	def apply(fmt:Format, fn:Option[()=>ScalaJack] = None) : ScalaJack = fmt match {
		case JSON => ScalaJack_JSON()
		case XML  => ScalaJack_XML()
//		case Custom => (fn.get)()
	}
}

trait ScalaJack {
	this: ReadRenderFrame =>
	def read[T](js:String, vctx:VisitorContext=VisitorContext())(implicit tt:TypeTag[T]) = {
		implicit val vc = vctx
		renderer.read(js)
	}
	def render[T](instance:T, vctx:VisitorContext=VisitorContext())(implicit tt:TypeTag[T]) = {
		implicit val vc = vctx
		renderer.render(instance)
	}
}

case class ScalaJack_JSON() extends ScalaJack with JSONReadRenderFrame 
case class ScalaJack_XML()  extends ScalaJack with XMLReadRenderFrame 
