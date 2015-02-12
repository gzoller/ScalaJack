package co.blocke.scalajack
package formats

import scala.reflect.runtime.universe._

object XML {
	// Map types to implicitly-assigned renderers
	def renderFarm[T](graph:SjType, instance:T, buf:StringBuilder)(implicit tt:TypeTag[T]) = {
		graph match {
			case g:SjCaseClass  => g.render(instance,buf)
			case g:SjPrimitive  => g.render(instance,buf)
			case g:SjCollection => g.render(instance,buf)
		}
	}

	implicit class CaseClassRender( val cc : SjCaseClass ) extends AnyVal {
		def render[T](instance:T, buf:StringBuilder)(implicit tt:TypeTag[T]):Boolean = {
			buf.append(s"""<class type="${cc.name}">""")
			cc.fields.foreach( f => {
				buf.append(s"""<${f.fieldName}>""")
				//render(f.ftype,instance)
				buf.append(s"""</${f.fieldName}>""")
			})
			buf.append("</class>")
			true
		}
	}
	implicit class TraitRender( val cc : SjTrait ) extends AnyVal {
		def render[T](instance:T, buf:StringBuilder)(implicit tt:TypeTag[T]):Boolean = {true}
	}
	implicit class CollectionRender( val cc : SjCollection ) extends AnyVal {
		def render[T](instance:T, buf:StringBuilder)(implicit tt:TypeTag[T]):Boolean = {true}
	}
	implicit class PrimitiveRender( val cc : SjPrimitive ) extends AnyVal {
		def render[T](instance:T, buf:StringBuilder)(implicit tt:TypeTag[T]):Boolean = {true}
	}
	implicit class TypeStmbolRender( val cc : SjTypeSymbol ) extends AnyVal {
		def render[T](instance:T, buf:StringBuilder)(implicit tt:TypeTag[T]):Boolean = {true}
	}
}