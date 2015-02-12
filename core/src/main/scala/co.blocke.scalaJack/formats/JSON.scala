package co.blocke.scalajack
package formats

import scala.reflect.runtime.universe._

object JSON {
	// Map types to implicitly-assigned renderers
	def renderFarm[T](graph:SjType, instance:T, buf:StringBuilder)(implicit tt:TypeTag[T]) = {
		graph match {
			case g:SjCaseClass  => g.render(instance,buf)
			case g:SjPrimitive  => g.render(instance,buf)
			case g:SjCollection => g.render(instance,buf)
		}
	}

	implicit class CaseClassRender( val cc:SjCaseClass ) extends AnyVal {
		def render[T](instance:T, buf:StringBuilder)(implicit tt:TypeTag[T]):Boolean = {
			buf.append("{")
			val sb2 = new StringBuilder()
			cc.fields.foreach( f => {
				val sb3 = new StringBuilder() // needed to support Option -- it may not render anything
				sb3.append(s""""${f.fieldName}":""")
				val cz = instance.getClass()
				val targetField = cz.getDeclaredField(f.fieldName)
				targetField.setAccessible(true)
				if( renderFarm(f.ftype, targetField.get(instance), sb3) ) {
					sb3.append(",")
					sb2.append(sb3)
				}
			})
			if( !sb2.isEmpty )
				buf.append(sb2.dropRight(1))
			buf.append("}")
			true
		}
	}
	implicit class TraitRender( val cc:SjTrait ) extends AnyVal {
		def render[T](instance:T, buf:StringBuilder)(implicit tt:TypeTag[T]):Boolean = { true }
	}
	implicit class CollectionRender( val cc:SjCollection ) extends AnyVal {
		def render[T](instance:T, buf:StringBuilder)(implicit tt:TypeTag[T]):Boolean = {
			cc.name match {
				case "scala.Option" => 
					val optVal = instance.asInstanceOf[Option[_]]
					optVal.map( ov => renderFarm(cc.collectionType.head, ov, buf) )
					optVal.isDefined
				case "scala.collection.immutable.Map" => 
					buf.append(s""""${instance}"""") //"
					true
				case _ => 
					buf.append("[")
					val collVal = instance.asInstanceOf[Iterable[_]]
					if( !collVal.isEmpty ) {
						collVal.map( item => {
							renderFarm(cc.collectionType.head, item, buf)
							buf.append(",")
						})
						if( buf.charAt(buf.length-1) == ',' )
							buf.deleteCharAt(buf.length-1)
					}
					buf.append("]")
					true
			}
		}
	}
	implicit class PrimitiveRender( val cc:SjPrimitive ) extends AnyVal {
		def render[T](instance:T, buf:StringBuilder)(implicit tt:TypeTag[T]):Boolean = {
			cc.name match {
				case "String" | "java.lang.String" | "scala.Char" => buf.append(s""""${instance}"""") //"
				case _ => buf.append(instance)
			}
			true
		}
	}
	implicit class TypeStmbolRender( val cc:SjTypeSymbol ) extends AnyVal {
		def render[T](instance:T, buf:StringBuilder)(implicit tt:TypeTag[T]):Boolean = { true }
	}
}
