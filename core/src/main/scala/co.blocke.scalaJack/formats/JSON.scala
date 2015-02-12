package co.blocke.scalajack
package formats

import scala.reflect.runtime.universe._

object JSON {
	implicit class CaseClassRender( val cc : SjCaseClass ) extends AnyVal {
		def render[T](instance:T)(implicit tt:TypeTag[T], buf:StringBuilder) = {
			buf.append("{")
			implicit val sb2 = new StringBuilder()
			cc.fields.foreach( f => {
				sb2.append(s""""${f.paramName}":""")
				//render(f.ftype,instance)
				sb2.append(",")
			})
			if( cc.fields.size > 0)
				buf.append(sb2.dropRight(1)) // trim last comma
			buf.append("}")
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
