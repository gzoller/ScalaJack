package co.blocke.scalajack
package json

import scala.reflect.runtime.universe._
import scala.collection.mutable.{Map => MMap,ListBuffer}
import JsonTokens._
import org.joda.time.DateTime
import scala.language.experimental.macros

/*
	OK, some wierd stuff goes on here...  Parameterized classes that have collections as their type pose real problems.
	The actual type information is available at the "parent" level (e.g. a case class field).  By the time you descend
	into the actual List type that information is lost.  So we have to always collect and pass our actual parameterized
	types in case they are needed by a collection. <sigh>  These are marked with //!!! for reference to this note.
 */

case class JsonKind() extends KindMarker  // For custom value class read/render (ValueClassCustom)

case class JsonFlavor() extends FlavorKind[String] {
	def makeScalaJack : ScalaJack[String] = new JsonScalaJack()  
	class JsonScalaJack() extends ScalaJack[String] with JsonJackFlavor
}

trait JsonJackFlavor extends JackFlavor[String] {
	def rr = new JsonReadRenderer()
	class JsonReadRenderer() extends ReadRenderer {
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

		def render[T](instance:T)(implicit tt:TypeTag[T], vc:VisitorContext=VisitorContext()) : String = {
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
						val hintValue = vc.hintValueRender.get(superTrait.name).map(_(g.name)).getOrElse(g.name)
						buf.append(s""""${vc.hintMap.getOrElse(superTrait.name,vc.hintMap("default"))}":"$hintValue"""") //"
						if(g.members.size > 0) buf.append(",")
					})
					val sb2 = new StringBuilder()
					g.members.foreach( { case(fname, (ftype,defaultVal)) => {
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
					buf.append( g.alias.flatMap( alias => vc.customHandlers.get(alias).map( _.render.applyOrElse( (JsonKind(), instance),
						(k:(KindMarker, _)) => throw new JsonParseException(s"No JSON read code provided in CustomReadRender handler for class ${g.name}",0)
					) ) ).getOrElse(
						g.name match {
							case "String" | "java.lang.String" | "scala.Char" | "scala.Enumeration.Value" | "java.util.UUID" if(instance != null) => 
								val cleaned = clean(instance.toString)
								s""""${cleaned}""""
							case "org.joda.time.DateTime" => instance.asInstanceOf[DateTime].getMillis.asInstanceOf[Long]
							case "scala.Any" => explodeAny(instance)
							case x => instance
						}					
					))
					true
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
									else g.colTypes(0) match {
										case PrimType("String",_) => sb3.append(s""""${k.toString}"""") //"
										case PrimType("java.lang.String",_) => sb3.append(s""""${k.toString}"""") //"
										case _ => throw new RenderException("Canonical JSON requires map keys to be of type String")
									}
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
						case t if( t startsWith("scala.Tuple") ) => 
							val arity = """\d+""".r.findFirstIn(t).get.toInt
							buf.append("[")
							arity match {
								case 2  => 
									val iv = instance.asInstanceOf[Tuple2[_,_]]
									_render(g.colTypes(0), iv._1, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(1), iv._2, buf, tt.tpe.typeArgs)
								case 3  => 
									val iv = instance.asInstanceOf[Tuple3[_,_,_]]
									_render(g.colTypes(0), iv._1, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(1), iv._2, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(2), iv._3, buf, tt.tpe.typeArgs)
								case 4  => 
									val iv = instance.asInstanceOf[Tuple4[_,_,_,_]]
									_render(g.colTypes(0), iv._1, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(1), iv._2, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(2), iv._3, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(3), iv._4, buf, tt.tpe.typeArgs)
								case 5  => 
									val iv = instance.asInstanceOf[Tuple5[_,_,_,_,_]]
									_render(g.colTypes(0), iv._1, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(1), iv._2, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(2), iv._3, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(3), iv._4, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(4), iv._5, buf, tt.tpe.typeArgs)
								case 6  => 
									val iv = instance.asInstanceOf[Tuple6[_,_,_,_,_,_]]
									_render(g.colTypes(0), iv._1, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(1), iv._2, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(2), iv._3, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(3), iv._4, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(4), iv._5, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(5), iv._6, buf, tt.tpe.typeArgs)
								case 7  => 
									val iv = instance.asInstanceOf[Tuple7[_,_,_,_,_,_,_]]
									_render(g.colTypes(0), iv._1, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(1), iv._2, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(2), iv._3, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(3), iv._4, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(4), iv._5, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(5), iv._6, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(6), iv._7, buf, tt.tpe.typeArgs)
								case 8  =>
									val iv = instance.asInstanceOf[Tuple8[_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), iv._1, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(1), iv._2, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(2), iv._3, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(3), iv._4, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(4), iv._5, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(5), iv._6, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(6), iv._7, buf, tt.tpe.typeArgs)								// case 9  => 
									buf.append(",")
									_render(g.colTypes(7), iv._8, buf, tt.tpe.typeArgs)
								case 9  => 
									val iv = instance.asInstanceOf[Tuple9[_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), iv._1, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(1), iv._2, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(2), iv._3, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(3), iv._4, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(4), iv._5, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(5), iv._6, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(6), iv._7, buf, tt.tpe.typeArgs)								// case 9  => 
									buf.append(",")
									_render(g.colTypes(7), iv._8, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(8), iv._9, buf, tt.tpe.typeArgs)
								case 10 => 
									val iv = instance.asInstanceOf[Tuple10[_,_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), iv._1, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(1), iv._2, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(2), iv._3, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(3), iv._4, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(4), iv._5, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(5), iv._6, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(6), iv._7, buf, tt.tpe.typeArgs)								// case 9  => 
									buf.append(",")
									_render(g.colTypes(7), iv._8, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(8), iv._9, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(9), iv._10, buf, tt.tpe.typeArgs)
								case 11 => 
									val iv = instance.asInstanceOf[Tuple11[_,_,_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), iv._1, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(1), iv._2, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(2), iv._3, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(3), iv._4, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(4), iv._5, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(5), iv._6, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(6), iv._7, buf, tt.tpe.typeArgs)								// case 9  => 
									buf.append(",")
									_render(g.colTypes(7), iv._8, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(8), iv._9, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(9), iv._10, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(10), iv._11, buf, tt.tpe.typeArgs)
								case 12 => 
									val iv = instance.asInstanceOf[Tuple12[_,_,_,_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), iv._1, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(1), iv._2, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(2), iv._3, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(3), iv._4, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(4), iv._5, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(5), iv._6, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(6), iv._7, buf, tt.tpe.typeArgs)								// case 9  => 
									buf.append(",")
									_render(g.colTypes(7), iv._8, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(8), iv._9, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(9), iv._10, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(10), iv._11, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(11), iv._12, buf, tt.tpe.typeArgs)
								case 13 => 
									val iv = instance.asInstanceOf[Tuple13[_,_,_,_,_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), iv._1, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(1), iv._2, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(2), iv._3, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(3), iv._4, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(4), iv._5, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(5), iv._6, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(6), iv._7, buf, tt.tpe.typeArgs)								// case 9  => 
									buf.append(",")
									_render(g.colTypes(7), iv._8, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(8), iv._9, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(9), iv._10, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(10), iv._11, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(11), iv._12, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(12), iv._13, buf, tt.tpe.typeArgs)
								case 14 => 
									val iv = instance.asInstanceOf[Tuple14[_,_,_,_,_,_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), iv._1, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(1), iv._2, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(2), iv._3, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(3), iv._4, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(4), iv._5, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(5), iv._6, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(6), iv._7, buf, tt.tpe.typeArgs)								// case 9  => 
									buf.append(",")
									_render(g.colTypes(7), iv._8, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(8), iv._9, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(9), iv._10, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(10), iv._11, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(11), iv._12, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(12), iv._13, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(13), iv._14, buf, tt.tpe.typeArgs)
								case 15 => 
									val iv = instance.asInstanceOf[Tuple15[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), iv._1, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(1), iv._2, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(2), iv._3, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(3), iv._4, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(4), iv._5, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(5), iv._6, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(6), iv._7, buf, tt.tpe.typeArgs)								// case 9  => 
									buf.append(",")
									_render(g.colTypes(7), iv._8, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(8), iv._9, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(9), iv._10, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(10), iv._11, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(11), iv._12, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(12), iv._13, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(13), iv._14, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(14), iv._15, buf, tt.tpe.typeArgs)
								case 16 => 
									val iv = instance.asInstanceOf[Tuple16[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), iv._1, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(1), iv._2, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(2), iv._3, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(3), iv._4, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(4), iv._5, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(5), iv._6, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(6), iv._7, buf, tt.tpe.typeArgs)								// case 9  => 
									buf.append(",")
									_render(g.colTypes(7), iv._8, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(8), iv._9, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(9), iv._10, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(10), iv._11, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(11), iv._12, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(12), iv._13, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(13), iv._14, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(14), iv._15, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(15), iv._16, buf, tt.tpe.typeArgs)
								case 17 => 
									val iv = instance.asInstanceOf[Tuple17[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), iv._1, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(1), iv._2, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(2), iv._3, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(3), iv._4, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(4), iv._5, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(5), iv._6, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(6), iv._7, buf, tt.tpe.typeArgs)								// case 9  => 
									buf.append(",")
									_render(g.colTypes(7), iv._8, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(8), iv._9, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(9), iv._10, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(10), iv._11, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(11), iv._12, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(12), iv._13, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(13), iv._14, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(14), iv._15, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(15), iv._16, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(16), iv._17, buf, tt.tpe.typeArgs)
								case 18 => 
									val iv = instance.asInstanceOf[Tuple18[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), iv._1, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(1), iv._2, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(2), iv._3, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(3), iv._4, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(4), iv._5, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(5), iv._6, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(6), iv._7, buf, tt.tpe.typeArgs)								// case 9  => 
									buf.append(",")
									_render(g.colTypes(7), iv._8, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(8), iv._9, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(9), iv._10, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(10), iv._11, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(11), iv._12, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(12), iv._13, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(13), iv._14, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(14), iv._15, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(15), iv._16, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(16), iv._17, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(17), iv._18, buf, tt.tpe.typeArgs)
								case 19 => 
									val iv = instance.asInstanceOf[Tuple19[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), iv._1, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(1), iv._2, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(2), iv._3, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(3), iv._4, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(4), iv._5, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(5), iv._6, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(6), iv._7, buf, tt.tpe.typeArgs)								// case 9  => 
									buf.append(",")
									_render(g.colTypes(7), iv._8, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(8), iv._9, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(9), iv._10, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(10), iv._11, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(11), iv._12, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(12), iv._13, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(13), iv._14, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(14), iv._15, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(15), iv._16, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(16), iv._17, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(17), iv._18, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(18), iv._19, buf, tt.tpe.typeArgs)
								case 20 => 
									val iv = instance.asInstanceOf[Tuple20[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), iv._1, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(1), iv._2, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(2), iv._3, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(3), iv._4, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(4), iv._5, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(5), iv._6, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(6), iv._7, buf, tt.tpe.typeArgs)								// case 9  => 
									buf.append(",")
									_render(g.colTypes(7), iv._8, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(8), iv._9, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(9), iv._10, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(10), iv._11, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(11), iv._12, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(12), iv._13, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(13), iv._14, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(14), iv._15, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(15), iv._16, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(16), iv._17, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(17), iv._18, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(18), iv._19, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(19), iv._20, buf, tt.tpe.typeArgs)
								case 21 => 
									val iv = instance.asInstanceOf[Tuple21[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), iv._1, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(1), iv._2, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(2), iv._3, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(3), iv._4, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(4), iv._5, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(5), iv._6, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(6), iv._7, buf, tt.tpe.typeArgs)								// case 9  => 
									buf.append(",")
									_render(g.colTypes(7), iv._8, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(8), iv._9, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(9), iv._10, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(10), iv._11, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(11), iv._12, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(12), iv._13, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(13), iv._14, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(14), iv._15, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(15), iv._16, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(16), iv._17, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(17), iv._18, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(18), iv._19, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(19), iv._20, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(20), iv._21, buf, tt.tpe.typeArgs)
								case 22 => 
									val iv = instance.asInstanceOf[Tuple22[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]]
									_render(g.colTypes(0), iv._1, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(1), iv._2, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(2), iv._3, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(3), iv._4, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(4), iv._5, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(5), iv._6, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(6), iv._7, buf, tt.tpe.typeArgs)								// case 9  => 
									buf.append(",")
									_render(g.colTypes(7), iv._8, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(8), iv._9, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(9), iv._10, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(10), iv._11, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(11), iv._12, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(12), iv._13, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(13), iv._14, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(14), iv._15, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(15), iv._16, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(16), iv._17, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(17), iv._18, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(18), iv._19, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(19), iv._20, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(20), iv._21, buf, tt.tpe.typeArgs)
									buf.append(",")
									_render(g.colTypes(21), iv._22, buf, tt.tpe.typeArgs)
							}
							buf.append("]")
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
					g.custom.map{ _.render.applyOrElse(
							(JsonKind(), renderVal ),
							(k:(KindMarker, _)) => _render(g.vcType,renderVal,buf,tt.tpe.typeArgs)
							)
					} match {
						case Some(x) => 
							buf.append(x)
							true
						case None => _render(g.vcType,renderVal,buf,tt.tpe.typeArgs)
					}
				case sj:JavaType =>
					val handler = vc.customHandlers.getOrElse(sj.name, throw new JsonParseException(s"No custom read/render handler (CustomReadRender) provided for class ${sj.name}",0))
					buf.append(handler.render.applyOrElse( (JsonKind(), instance),
						(k:(KindMarker, _)) => throw new JsonParseException(s"No JSON render code provided in CustomReadRender handler for class ${sj.name}",0)
					).toString)
					true
				case g:EnumType =>
					buf.append(s""""${instance.toString}"""") //"
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