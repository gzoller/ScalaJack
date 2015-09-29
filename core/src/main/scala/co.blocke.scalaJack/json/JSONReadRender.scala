package co.blocke.scalajack
package json

import scala.reflect.runtime.universe._
import scala.collection.mutable.{Map => MMap,ListBuffer}
import JsonTokens._
import org.joda.time.DateTime

/*
	OK, some wierd stuff goes on here...  Parameterized classes that have collections as their type pose real problems.
	The actual type information is available at the "parent" level (e.g. a case class field).  By the time you descend
	into the actual List type that information is lost.  So we have to always collect and pass our actual parameterized
	types in case they are needed by a collection. <sigh>  These are marked with //!!! for reference to this note.
 */

trait JSONReadRenderFrame extends ReadRenderFrame[String] { 
	def renderer = new JSONReadRender()

	class JSONReadRender() extends ReadRender {

		def read[T](src:String)(implicit tt:TypeTag[T], vc:VisitorContext=VisitorContext()) : T = {
			val sjTypeName = tt.tpe.typeSymbol.fullName
			val srcChars = src.toCharArray
			val parser = vc.isValidating match {
				case true if(vc.isCanonical)  => JsonParser(sjTypeName, ValidTokenizer().tokenize(srcChars),                 vc)
				case true if(!vc.isCanonical) => JsonParser(sjTypeName, ValidTokenizer(false).tokenize(srcChars),            vc)
				case false                    => JsonParser(sjTypeName, FastTokenizer(vc.estFieldsInObj).tokenize(srcChars), vc)
			}
			parser.parse()
		}

		def render[T](instance:T)(implicit tt:TypeTag[T], vc:VisitorContext) : String = {
			val buf = new StringBuilder()
			_render(Analyzer.inspect(instance), instance, buf)
			buf.toString
		}

		private def _render[T](
			graph    : AType, 
			instance : T, 
			buf      : StringBuilder, 
			typeArgs : List[Type]=List.empty[Type]
		)(implicit tt:TypeTag[T], vc:VisitorContext):Boolean = {
			graph match {
				case _ if( instance == null ) => 
					buf.append("null")
					true
				case g:CCType  => 
					buf.append("{")
					g.superTrait.map( superTrait => {
						buf.append(s""""${vc.hintMap.getOrElse(superTrait.name,vc.hintMap("default"))}":"${g.name}"""") //"
						if(g.members.size > 0) buf.append(",")
					})
					val sb2 = new StringBuilder()
					g.members.foreach( { case(fname, ftype) => {
						val sb3 = new StringBuilder() // needed to support Option -- it may not render anything
						sb3.append(s""""${fname}":""")
						val cz = instance.getClass()
						val targetField = cz.getDeclaredField(fname)
						targetField.setAccessible(true)
						if( _render(ftype, targetField.get(instance), sb3, tt.tpe.typeArgs) ) {
							sb3.append(",")
							sb2.append(sb3)
						}
					}})
					if( !sb2.isEmpty )
						buf.append(sb2.dropRight(1))
					buf.append("}")
					true
				case g:PrimType  => 
					g.name match {
						case "String" | "java.lang.String" | "scala.Char" | "scala.Enumeration.Value" | "java.util.UUID" if(instance != null) => 
							val cleaned = clean(instance.toString)
							buf.append(s""""${cleaned}"""") //"
							true
						case "org.joda.time.DateTime" =>
							buf.append(instance.asInstanceOf[DateTime].getMillis.asInstanceOf[Long])
							true
						case "scala.Any" => 
							buf.append( explodeAny(instance) )
							true
						case x => 
							buf.append(instance)
							true
					}
				case g:CollType =>
					g.name match {
						case "scala.Option" => 
							val optVal = instance.asInstanceOf[Option[_]]
							optVal.map( ov => _render(g.colTypes.head, ov, buf, tt.tpe.typeArgs) )
							optVal.isDefined
						case n if(n.endsWith("Map")) => 
							val mapVal = instance.asInstanceOf[Map[_,_]]
							buf.append("{")
							if( !mapVal.isEmpty ) 
								mapVal.map({ case (k,v) => {
									val sb3 = new StringBuilder()
									var renderedKey = true // handle optionality
									if( !vc.isCanonical ) 
										renderedKey = _render(g.colTypes(0), k, sb3, tt.tpe.typeArgs)
									else 
										sb3.append(s""""${k.toString}"""") //"
									if( renderedKey ) {
										sb3.append(":")
										if( _render(g.colTypes(1), v, sb3, tt.tpe.typeArgs) )
											sb3.append(",")
										else
											sb3.clear
									} else
										sb3.clear
									buf.append(sb3)
									}})
							if( buf.charAt(buf.length-1) == ',' )
								buf.deleteCharAt(buf.length-1)
							buf.append("}")
							true
						case _ => 
							buf.append("[")
							val collVal = instance.asInstanceOf[Iterable[_]]
							if( !collVal.isEmpty ) {
								collVal.map( item => {
									if( _render(g.colTypes.head, item, buf, tt.tpe.typeArgs) )
										buf.append(",")
								})
								if( buf.charAt(buf.length-1) == ',' )
									buf.deleteCharAt(buf.length-1)
							}
							buf.append("]")
							true
					}
				case g:TraitType => 
					val cc = Analyzer.inspect(instance,Some(g)).asInstanceOf[CCType]
					_render(cc.copy(superTrait=Some(g)),instance,buf, tt.tpe.typeArgs)
				case g:ValueClassType =>
					// Value classes are ugly!  Sometimes they're inlined so don't assume a class here... it may be just
					// a raw/unwrapped value.  But... other times they are still wrapped in their class.  Be prepared
					// to handle either.
					//
					// NOTE: Will not handle parameterized value classes having a non-primitive parameter, e.g. List
					val renderVal = {
						if( g.name != instance.getClass.getName ) // raw/unwrapped value
							instance
						else {
							val targetField = instance.getClass.getDeclaredField(g.vFieldName)
							targetField.setAccessible(true)
							targetField.get(instance)
						}
					}
					vc.valClassMap.get(g.name).map( vcHand => {
							buf.append(vcHand.render(renderVal))
							true
						}).orElse(
							Some(_render(g.vcType,renderVal,buf,tt.tpe.typeArgs))
						).get
				case g:EnumType =>
					buf.append(s""""${instance.toString}"""") //"
					true
				case g:CustomType =>
					buf.append( g.renderers("default")(instance) )
					true
			}
		}

		private def explodeAny( inst:Any ) : String = inst match {
			case s:String   => s""""$s""""
			case l:List[_]  => l.map( explodeAny(_) ).mkString("[",",","]")
			case m:Map[_,_] => m.map( { case(k,v) => '"'+k.toString+"\":"+explodeAny(v)} ).mkString("{",",","}")
			case n if(inst == null) => "null"
			case x          => 	x.toString
		}
	}
}