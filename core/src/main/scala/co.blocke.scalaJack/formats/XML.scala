package co.blocke.scalajack
package formats

import scala.reflect.runtime.universe._

trait XMLReadRenderFrame extends ReadRenderFrame {
	def renderer = new XMLReadRender()

	class XMLReadRender() extends ReadRender[String] {
		def render[T](instance:T)(implicit tt:TypeTag[T], vc:VisitorContext) : String = {
			val graph = getGraph(instance)
			val buf = new StringBuilder()
			_render(graph, instance, buf)
			buf.toString
		}

		private def _render[T](
			graph:SjType, 
			instance:T, 
			buf:StringBuilder, 
			typeArgs:List[Type]=List.empty[Type]
		)(implicit tt:TypeTag[T], vc:VisitorContext):Boolean = 
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
							if( _render(f.ftype, instVal, sb3, tt.tpe.typeArgs) ) {
								sb3.append(s"""</field>""")
								buf.append(sb3)
							}
						}
					})
					buf.append("</class>")
					true
				case g:SjPrimitive  => 
					if(g.name == "scala.Any") 
						_render(Analyzer.inspect(instance),instance,buf,tt.tpe.typeArgs)
					else {
						buf.append(instance)
						true
					}
				case g:SjCollection => 
					g.name match {
						case "scala.Option" => 
							val optVal = instance.asInstanceOf[Option[_]]
							optVal.map( ov => _render(g.collectionType.head, ov, buf, tt.tpe.typeArgs) )
							optVal.isDefined
						case n if(n.endsWith("Map")) =>  
							val mapVal = instance.asInstanceOf[Map[_,_]]
							if( mapVal.isEmpty ) 
								buf.append(s"""<map class="${g.name}"/>""")
							else {
								buf.append(s"""<map class="${g.name}">""")
								mapVal.map({ case (k,v) => {
									val sb3 = new StringBuilder()
									sb3.append("<entry>")
									var renderedKey = true // handle optionality
									if( vc.sloppyJSON ) { 
										sb3.append("<key>")
										renderedKey = _render(g.collectionType(0), k, sb3, tt.tpe.typeArgs)
										sb3.append("</key>")
									} else
										sb3.append(s"""<key>${k.toString}</key>""") //"
									if( renderedKey ) {
										sb3.append("<value>")
										if( _render(g.collectionType(1), v, sb3, tt.tpe.typeArgs) ) {
											sb3.append("</value>")
											sb3.append("</entry>")
										} else
											sb3.clear
									} else
										sb3.clear
									buf.append(sb3)
									}})
								if( buf.charAt(buf.length-1) == ',' )
									buf.deleteCharAt(buf.length-1)
								buf.append("</map>")
							}
							true
						case _ => 
							buf.append(s"""<list class="${g.name}">""")
							val collVal = instance.asInstanceOf[Iterable[_]]
							if( !collVal.isEmpty ) {
								collVal.map( item => {
									val sb3 = new StringBuilder()
									sb3.append("<item>")
									if( _render(g.collectionType.head, item, sb3, tt.tpe.typeArgs) ) {
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
				case g:SjTypeSymbol => 
					val analyzed = Analyzer.inspect(instance) match {
						// naked list = must supply actual collection type
						case c:SjCollection if(c.collectionType.size==0) => Analyzer.nakedInspect(typeArgs).head
						case c => c
					}
					_render(analyzed,instance,buf,tt.tpe.typeArgs)
				case g:SjTrait      => 
					val cc = Analyzer.inspect(instance).asInstanceOf[SjCaseClass]
					// WARN: Possible Bug.  Check propagation of type params from trait->case class.  These may need
					//       to be intelligently mapped somehow.
					_render(cc.copy(isTrait=true, params=g.params),instance,buf, tt.tpe.typeArgs)
			}
	}
}