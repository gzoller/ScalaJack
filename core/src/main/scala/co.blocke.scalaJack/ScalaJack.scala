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
 *         Map  (mutable and immutable)
 *         Option
 *   -- MongoKey-decorated fields must be type String and map to "_id" (Mongo's default)
 *   
 * Useful references:
 * 		http://stackoverflow.com/questions/17006271/deep-access-of-fields-in-scala-using-runtime-reflection
 */

import scala.reflect.runtime.universe._

object Formats extends Enumeration {
	type Format = Value
	val JSON, XML, Custom = Value
}
import Formats._
  
object ScalaJack {
	def apply(fmt:Format, fn:Option[()=>ScalaJack] = None) = fmt match {
		case JSON => ScalaJack_JSON()
		case XML  => ScalaJack_XML()
		case Custom => (fn.get)()
	}
}

trait ScalaJack {
	def render[T]( instance:T )(implicit tt:TypeTag[T]) = {
		val graph = Analyzer.inspect(instance)
		implicit val buf = new StringBuilder()
		_render(graph, instance )
		buf.toString
	}
	protected def _render[T]( graph:SjType, instance:T )(implicit tt:TypeTag[T], buf:StringBuilder)
}

case class ScalaJack_JSON() extends ScalaJack {
	import formats.JSON._
	protected def _render[T]( graph:SjType, instance:T )(implicit tt:TypeTag[T], buf:StringBuilder) =
		graph match {
		  case g:SjCaseClass => g.render(instance)
		}    
}

case class ScalaJack_XML() extends ScalaJack {
	import formats.XML._
	protected def _render[T]( graph:SjType, instance:T )(implicit tt:TypeTag[T], buf:StringBuilder) =
		graph match {
		  case g:SjCaseClass => g.render(instance)
		}
}