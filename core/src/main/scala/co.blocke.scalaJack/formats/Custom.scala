package co.blocke.scalajack
package formats

import scala.reflect.runtime.universe._

object Custom {
	// Map types to implicitly-assigned renderers
	def renderFarm[T](graph:SjType, instance:T, buf:StringBuilder)(implicit tt:TypeTag[T]) = {
		graph match {
			case g:SjCaseClass  => g.render(instance,buf)
		}
	}
	implicit class CaseClassRender( val cc : SjCaseClass ) extends AnyVal {
		def render[T](instance:T, buf:StringBuilder)(implicit tt:TypeTag[T]):Boolean = {
			buf.append("{{")
			implicit val sb2 = new StringBuilder()
			cc.fields.foreach( f => {
				sb2.append(s""""${f.fieldName}":""")
				//render(f.ftype,instance)
				sb2.append(",")
			})
			if( cc.fields.size > 0)
				buf.append(sb2.dropRight(1)) // trim last comma
			buf.append("}}")
			true
		}
	}
}