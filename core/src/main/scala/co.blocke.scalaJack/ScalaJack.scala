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

trait FlavorKind[S] {
	def makeScalaJack : ScalaJack[S]
}

trait ScalaJack[S] {
	this : JackFlavor[S] =>  // self-type part of Cake Pattern!
	private val defaultVC = VisitorContext()
	def read[T](src:S, vctx:VisitorContext=defaultVC)(implicit tt:TypeTag[T]) : T = {
		implicit val vc = vctx
		rr.read(src)
	}
	def render[T](instance:T, vctx:VisitorContext=defaultVC)(implicit tt:TypeTag[T]) : S = {
		implicit val vc = vctx
		rr.render(instance)
	}
}

object ScalaJack extends ViewSplice {
	def apply[R]( flavor:FlavorKind[R] = JsonFlavor() ) = flavor.makeScalaJack
}

//-----------------------------------------------------------

// MOVE TO UTIL CLASS OR SOMETHING!
object JSON {
	def toCollection( js:String, size:Int = 500 ) : Either[Map[String,Any],List[Any]] = json.FastTokenizer(size).tokenize(js.toCharArray).toCollection()
}
