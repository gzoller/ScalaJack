package co.blocke.scalajack
package json

import scala.reflect.runtime.universe._
import scala.collection.mutable.{Map => MMap,ListBuffer}
import JsonTokens._

/*
	OK, some wierd stuff goes on here...  Parameterized classes that have collections as their type pose real problems.
	The actual type information is available at the "parent" level (e.g. a case class field).  By the time you descend
	into the actual List type that information is lost.  So we have to always collect and pass our actual parameterized
	types in case they are needed by a collection. <sigh>  These are marked with //!!! for reference to this note.
 */

case class ParseFrame() {
	var inObjVal  : Boolean         = false
	var objKey    : Any             = null
	val objFields : MMap[Any,Any]   = MMap.empty[Any,Any]
	val listItems : ListBuffer[Any] = ListBuffer.empty[Any]
	var item      : SjItem          = null
}

trait JSONReadRenderFrame extends ReadRenderFrame {
	def renderer = new JSONReadRender()

	class JSONReadRender() extends ReadRender[String] {

		/**
		 * Magically create an instance of a case class given a map of name->value parameters.
		 * (Reflects on the apply method of the case class' companion object.)
		 * It's a quick way to materialize a cse class represented by a Map, which is how ScalaJack uses it.
		 * ScalaJack parses the JSON, building a value Map as it goes.  When the JSON object has been parsed
		 * ScalaJack calls poof to build the case class from the Map.
		 */
/*
		private[scalajack] def poof( cc:SjCaseClass, data:Map[String,Any] ) : Any = {
			val args = cc.fields.collect{ case f => data.get(f.name).getOrElse(None) }.toArray.asInstanceOf[Array[AnyRef]]
			cc.applyMethod.invoke( classField.caseObj, args:_* )
		}

		def read[T](src:String)(implicit tt:TypeTag[T], vc:VisitorContext=VisitorContext()) : T = {
// This is just for dev purposes...final must be more flexible
val fast = FastTokenizer(128)
val srcChars = src.toCharArray
val idx = fast.tokenize(srcChars)

val item = ??? // Analyze given type T (as SjType)

			// Internal function so we don't pass around a lot of large data structures
			def visit( i:Int, sjtype:SjType ) : (Int,Any) = sjtype match {
				case st:SjCollection =>
					idx.tokType(i) match {
						case JSlistStart =>  // list-ish collection
							val listBuf = ListBuffer.empty[Any]
							var pc = i+1
							while( idx.tokType(pc) != JSlistEnd && idx.tokType(pc) != JSlistEndInList ) {
								val (iLater,baked) = visit(pc, st.collectionType(0))
								pc = iLater+1
								listBuf += baked
							}
							(pc, listBuff.toSeq)
						case JSobjStart =>  // map-ish collection
						case _   => throw new JsonParseException("Expected collection but found JSON token ${JsonTokens.toName(idx.tokType(i))} at position ${idx.tokPos(i)}",0)
					}
			}

val (i,result) = visit(0)
result.asInstanceOf[T]

			val frameStack = scala.collection.mutable.Stack.empty[ParseFrame]
			var finalResult:Any = null
			(0 to idx.tokCount).foreach( i => idx.tokType(i) match {
				case JSobjStart      => 
					frameStack.push(ParseFrame())
					frameStack.head.item = ??? // SjType (ensure SjCaseClass or SjCollection map flavor, or error) gotten by Analyzer
				case JSobjEnd        => 
					val frame = frameStack.pop 
					val materialized = {
						if( frame.item.isInstanceOf[SjCaseClass] )  
							poof( frame.item.asInstanceOf[SjCaseClass], data??? )
						// else ???
					}
					if( frameStack.size == 0 )  // all done
						finalResult = materialized
					// else ???
				case JSobjEndInList  =>
				case JSobjEndObjKey  =>
				case JSlistStart     =>
				case JSlistEnd       =>
				case JSlistEndInList =>
				case JSlistEndObjKey =>
				case JStrue          =>
					if(frameStack.head.inObjVal) {
						frameStack.head.inObjVal = false
						frameStack.head.objFields.put(frameStack.head.objKey, true)
					}
				case JStrueInList    =>
				case JSfalse         =>
					if(frameStack.head.inObjVal) {
						frameStack.head.inObjVal = false
						frameStack.head.objFields.put(frameStack.head.objKey, false)
					}
				case JSfalseInList   =>
				case JSnull          =>
					if(frameStack.head.inObjVal) {
						frameStack.head.inObjVal = false
						frameStack.head.objFields.put(frameStack.head.objKey, null)
					}
				case JSnullInList    =>
				case JSstring        =>
					if(frameStack.head.inObjVal) {
						frameStack.head.inObjVal = false
						frameStack.head.objFields.put(frameStack.head.objKey, idx.getToken(i,srcChars))
					}
				case JSstringInList  =>
				case JSstringObjKey  => 
					frameStack.head.objKey = idx.getToken(i,srcChars)
					frameStack.head.inObjVal = true
				case JSnumber        =>
				case JSnumberInList  =>
				case JSnumberObjKey  =>
			})
			finalResult.asInstanceOf[T]
		}
		*/

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
						case "String" | "java.lang.String" | "scala.Char" | "scala.Enumeration.Value" | "java.util.UUID" => 
							val cleaned = clean(instance.toString)
							buf.append(s""""${cleaned}"""") //"
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
						case n if(n.endsWith("Map")) => 
							val mapVal = instance.asInstanceOf[Map[_,_]]
							buf.append("{")
							if( !mapVal.isEmpty ) 
								mapVal.map({ case (k,v) => {
									val sb3 = new StringBuilder()
									var renderedKey = true // handle optionality
									if( vc.sloppyJSON ) 
										renderedKey = _render(g.collectionType(0), k, sb3, tt.tpe.typeArgs)
									else
										sb3.append(s""""${k.toString}"""") //"
									if( renderedKey ) {
										sb3.append(":")
										if( _render(g.collectionType(1), v, sb3, tt.tpe.typeArgs) )
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
						case c:SjCollection if(c.collectionType.size==0) => 
							Analyzer.nakedInspect(typeArgs).head
						case c => c
					}
					_render(analyzed,instance,buf, tt.tpe.typeArgs)
				case g:SjTrait => 
					val cc = Analyzer.inspect(instance).asInstanceOf[SjCaseClass]
					// WARN: Possible Bug.  Check propagation of type params from trait->case class.  These may need
					//       to be intelligently mapped somehow.
					_render(cc.copy(isTrait=true, params=g.params),instance,buf, tt.tpe.typeArgs)
				case g:SjValueClass =>
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
					_render(g.vcType,renderVal,buf,tt.tpe.typeArgs)
			}
		}
	}
}