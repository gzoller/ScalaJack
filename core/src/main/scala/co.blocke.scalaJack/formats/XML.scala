package co.blocke.scalajack
package formats

import scala.reflect.runtime.universe._

object XML {
	implicit class CaseClassRender( val cc : SjCaseClass ) extends AnyVal {
		def render[T](instance:T)(implicit tt:TypeTag[T], buf:StringBuilder) = {
			buf.append(s"""<class type="${cc.name}">""")
			cc.fields.foreach( f => {
				buf.append(s"""<${f.paramName}>""")
				//render(f.ftype,instance)
				buf.append(s"""</${f.paramName}>""")
			})
			buf.append("</class>")
		}
	}
	implicit class TraitRender( val cc : SjTrait ) extends AnyVal {
		def render[T](instance:T)(implicit tt:TypeTag[T], buf:StringBuilder) = {}
	}
	implicit class CollectionRender( val cc : SjCollection ) extends AnyVal {
		def render[T](instance:T)(implicit tt:TypeTag[T], buf:StringBuilder) = {}
	}
	implicit class PrimitiveRender( val cc : SjPrimitive ) extends AnyVal {
		def render[T](instance:T)(implicit tt:TypeTag[T], buf:StringBuilder) = {}
	}
	implicit class TypeStmbolRender( val cc : SjTypeSymbol ) extends AnyVal {
		def render[T](instance:T)(implicit tt:TypeTag[T], buf:StringBuilder) = {}
	}
}