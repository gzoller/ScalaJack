package co.blocke.scalajack
package formats

import scala.reflect.runtime.universe._

trait XMLReadRenderFrame extends ReadRenderFrame {
	def renderer = new XMLReadRender()

	class XMLReadRender() extends ReadRender[String] {
		def render[T](instance:T)(implicit tt:TypeTag[T]) : String = {
			val graph = Analyzer.inspect(instance)
			val buf = new StringBuilder()
			_render(graph, instance, buf)
			buf.toString
		}

		private def _render[T](graph:SjType, instance:T, buf:StringBuilder)(implicit tt:TypeTag[T]):Boolean = 
			graph match {
				case g:SjCaseClass  => 
					buf.append(s"""<class type="${g.name}">""")
					g.fields.foreach( f => {
						val sb3 = new StringBuilder() // needed to support Option -- it may not render anything
						val cz = instance.getClass()
						val targetField = cz.getDeclaredField(f.fieldName)
						targetField.setAccessible(true)
						val instVal = targetField.get(instance)
						if( instVal == null ) // special XML handling of null
							buf.append(s"""<field name="${f.fieldName}" xsi:nil="true"/>""")
						else {
							sb3.append(s"""<field name="${f.fieldName}">""")
							if( _render(f.ftype, instVal, sb3) ) {
								sb3.append(s"""</field>""")
								buf.append(sb3)
							}
						}
					})
					buf.append("</class>")
					true
				case g:SjPrimitive  => 
					buf.append(instance)
					true
				case g:SjCollection => 
					g.name match {
						case "scala.Option" => 
							val optVal = instance.asInstanceOf[Option[_]]
							optVal.map( ov => _render(g.collectionType.head, ov, buf) )
							optVal.isDefined
						case "scala.collection.immutable.Map" => 
							buf.append(s""""${instance}"""") //"
							true
						case _ => 
							buf.append(s"""<list class="${g.name}">""")
							val collVal = instance.asInstanceOf[Iterable[_]]
							if( !collVal.isEmpty ) {
								collVal.map( item => {
									val sb3 = new StringBuilder()
									sb3.append("<item>")
									if( _render(g.collectionType.head, item, sb3) ) {
										sb3.append("</item>")
										buf.append(sb3)
									}
								})
								if( buf.charAt(buf.length-1) == ',' )
									buf.deleteCharAt(buf.length-1)
							}
							buf.append("</list>")
							true
					}
				case g:SjTrait      => true
				case g:SjTypeSymbol => true
			}
	}
}