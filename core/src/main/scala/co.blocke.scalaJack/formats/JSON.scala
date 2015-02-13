package co.blocke.scalajack
package formats

import scala.reflect.runtime.universe._

/*
	OK, some wierd stuff goes on here...  Parameterized classes that have collections as their type pose real problems.
	The actual type information is available at the "parent" level (e.g. a case class field).  By the time you descend
	into the actual List type that information is lost.  So we have to always collect and pass our actual parameterized
	types in case they are needed by a collection. <sigh>  These are marked with //!!! for reference to this note.
 */

trait JSONReadRenderFrame extends ReadRenderFrame {
	def renderer = new JSONReadRender()

	class JSONReadRender() extends ReadRender[String] {
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
		)(implicit tt:TypeTag[T], vc:VisitorContext):Boolean = {
			graph match {
				case g:SjCaseClass  => 
					buf.append("{")
					if( g.isTrait ) {
						buf.append(s""""${vc.traitHintLabel}":"${g.name}"""") //"
						if(g.fields.size > 0) buf.append(",")
					}
					val sb2 = new StringBuilder()
					g.fields.foreach( f => {
						val sb3 = new StringBuilder() // needed to support Option -- it may not render anything
						sb3.append(s""""${f.fieldName}":""")
						val cz = instance.getClass()
						val targetField = cz.getDeclaredField(f.fieldName)
						targetField.setAccessible(true)
						if( _render(f.ftype, targetField.get(instance), sb3, tt.tpe.typeArgs) ) {
							sb3.append(",")
							sb2.append(sb3)
						}
					})
					if( !sb2.isEmpty )
						buf.append(sb2.dropRight(1))
					buf.append("}")
					true
				case g:SjPrimitive  => 
					g.name match {
						case "String" | "java.lang.String" | "scala.Char" | "scala.Enumeration.Value" => 
							buf.append(s""""${instance}"""") //"
							true
						case "scala.Any" => _render(Analyzer.inspect(instance),instance,buf)
						case _ => 
							buf.append(instance)
							true
					}
				case g:SjCollection => 
					g.name match {
						case "scala.Option" => 
							val optVal = instance.asInstanceOf[Option[_]]
							optVal.map( ov => _render(g.collectionType.head, ov, buf, tt.tpe.typeArgs) )
							optVal.isDefined
						case "scala.collection.immutable.Map" => 
							val mapVal = instance.asInstanceOf[Map[_,_]]
							if( mapVal.isEmpty ) buf.append("{}") 
							true
						case _ => 
							buf.append("[")
							val collVal = instance.asInstanceOf[Iterable[_]]
							if( !collVal.isEmpty ) {
								collVal.map( item => {
									if( _render(g.collectionType.head, item, buf, tt.tpe.typeArgs) )
										buf.append(",")
								})
								if( buf.charAt(buf.length-1) == ',' )
									buf.deleteCharAt(buf.length-1)
							}
							buf.append("]")
							true
					}
				case g:SjTypeSymbol =>
					val analyzed = Analyzer.inspect(instance) match {
						// naked list = must supply actual collection type
						case c:SjCollection if(c.collectionType.size==0) => Analyzer.nakedInspect(typeArgs).head
						case c => c
					}
					_render(analyzed,instance,buf, tt.tpe.typeArgs)
				case g:SjTrait => 
					val cc = Analyzer.inspect(instance).asInstanceOf[SjCaseClass]
					// WARN: Possible Bug.  Check propagation of type params from trait->case class.  These may need
					//       to be intelligently mapped somehow.
					_render(cc.copy(isTrait=true, params=g.params),instance,buf, tt.tpe.typeArgs)
			}
		}
	}
}