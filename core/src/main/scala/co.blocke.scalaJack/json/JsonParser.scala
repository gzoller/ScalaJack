package co.blocke.scalajack
package json

import scala.reflect.runtime.currentMirror
import PrimitiveTypes._
import scala.reflect.runtime.universe._
import JsonTokens._
import scala.collection.mutable.{Map => MMap,ListBuffer => MList}

case class JsonParser(sjTName:String, idx:JsonIndex, vctx:VisitorContext) {

	def parse[T]()(implicit tt:TypeTag[T]) : T = {
		var i = 0  // index into idx

		def _makeClass[U]( sjT:CCType, t:AType )(implicit ty:TypeTag[U]) = {
			val objFields = MMap.empty[Any,Any]
			if( idx.tokType(i-1) != JSobjStart ) throw new JsonParseException(s"Expected JSobjStart and found ${JsonTokens.toName(idx.tokType(i-1))} at token $i",0)

			// read key/value pairs
			while( idx.tokType(i) != JSobjEnd && idx.tokType(i) != JSobjEndInList  && idx.tokType(i) != JSobjEndObjKey) {
				if( idx.tokType(i) != JSstringObjKey ) throw new JsonParseException(s"Expected JSstringObjKey and found ${JsonTokens.toName(idx.tokType(i))}",0)
				val fieldName = idx.getToken(i)
				i += 1
				// The "f._2._1" here gets the member's AType while ignoring the default value
				sjT.members.find(_._1 == fieldName).fold( skipValue )( f => objFields.put(fieldName, _parse(f._2._1)) )
			}
			i+=1
			sjT.materialize(objFields.toMap.asInstanceOf[Map[String,Any]])
		}

		def _parse( t:AType, topLevel:Boolean = false ) : Any = t match {
			case sj:CCType =>
				i += 1
				_makeClass(sj, t)

			case sj:TraitType =>
				i += 1
				val sjCC = {
					// Look-ahead and find type hint--figure out what kind of object his is and inspect it.
					val objClass = findTypeHint(vctx.hintMap.getOrElse(sj.name,vctx.hintMap("default")))
						// See if we need to look up actual objClass (e.g. abbreviation) or if its ready-to-eat
						.map( candidate => vctx.hintValueRead.get(sj.name).map(_(candidate)).getOrElse(candidate) )
					if( !objClass.isDefined )
						throw new JsonParseException(s"No type hint given for trait ${sj.name}",0)
					val sjObjType = Analyzer.inspectByName(objClass.get.toString,Some(sj))
					if( !sjObjType.isInstanceOf[CCType] ) throw new JsonParseException(s"Type hint $objClass does not specify a case class",0)
					sjObjType.asInstanceOf[CCType]
				}
				_makeClass(sjCC,t)

			case sj:PrimType =>
				val v = idx.tokType(i) match {
					case pt if(sj.primCode == PrimitiveTypes.ANY) => // scala.Any
						val (newI, value) = inferSimpleType(idx,i)
						i = newI-1
						value
					case JSstringObjKey | JSstring | JSstringInList | JSnumberObjKey | JSnumber | JSnumberInList =>
						val text = Unicode.unescape_perl_string(idx.getToken(i))
						sj.alias.flatMap( alias => vctx.customHandlers.get(alias).map( _.read.applyOrElse( (JsonKind(), text),
							(k:(KindMarker, _)) => throw new JsonParseException(s"No JSON read code provided in CustomReadRender handler for class ${sj.name}",0)
						) ) ).getOrElse(
							PrimitiveTypes.primTypes(sj.primCode)( text )
						)
					case JStrue  | JStrueInList | JStrueObjKey  => true
					case JSfalse | JSfalseInList | JSfalseObjKey => false
					case JSnull  | JSnullInList  => null
					case _ => throw new JsonParseException(s"Expected primitive value but saw ${JsonTokens.toName(idx.tokType(i))} obj ${idx.getToken(i)}",0)
				}
				i += 1
				v

			case sj:EnumType =>
				try {
					sj.enum.withName( idx.getToken(i) )
				} catch {
					case w:Throwable => throw new JsonParseException(s"Value ${idx.getToken(i)} is not a valid for enum ${sj.enum.toString}",0)
				} finally {
					i += 1
				}

			case sj:CollType =>
				val ret = { 
					if(idx.tokType(i) == JSnull) 
						null
					else if( sj.isOptional ) {
						val parsed = _parse(sj.colTypes(0))
						i -= 1  // compensate for increment later
						Some(parsed)
					} else if( sj.collCode > 0 && sj.collCode < 10 ) {  // range in PrimitiveTypes for Map variants
						val mapAcc = MMap.empty[Any,Any]
						if( idx.tokType(i) != JSobjStart ) 
							throw new JsonParseException(s"Expected JSlistStart and found ${JsonTokens.toName(idx.tokType(i))}",0)
						i += 1
						sj.colTypes(0) match { // For canonical JSON Map key must resolve to String type.  Anything goes for non-canoical.
							case ct if(!vctx.isCanonical) =>
							case ct:PrimType if(ct.primCode == PrimitiveTypes.STRING) =>
							case et:EnumType => 
							case ValueClassType(_,PrimType("String",_),_,_,_) | ValueClassType(_,PrimType("java.lang.String",_),_,_,_) | ValueClassType(_,EnumType(_,_),_,_,_) =>
							case t => throw new JsonParseException("Map keys must be of type String in canonical JSON",0)
						}
						while( idx.tokType(i) != JSobjEnd && idx.tokType(i) != JSobjEndInList && idx.tokType(i) != JSobjEndObjKey ) {
							val key = _parse(sj.colTypes(0))
							val value = _parse(sj.colTypes(1)) 
							mapAcc.put(key,value)
						}
						PrimitiveTypes.collTypes(sj.collCode)(mapAcc.toList)
					} else if(sj.collCode > 29) {  // range in PrimitiveTypes for Tuple variants
						val arity = """\d+""".r.findFirstIn(sj.name).get.toInt
						if( idx.tokType(i) != JSlistStart ) 
							throw new JsonParseException(s"Expected JSlistStart and found ${JsonTokens.toName(idx.tokType(i))}",0)
						i += 1
						val tv = (0 to arity-1).map( a => _parse(sj.colTypes(a)) ) // parse tuple values
						if( idx.tokType(i) != JSlistEnd && idx.tokType(i) != JSlistEndInList && idx.tokType(i) != JSlistEndObjKey  ) 
							throw new JsonParseException(s"Expected JSlistEnd or JSlistEndInList and found ${JsonTokens.toName(idx.tokType(i))}",0)
						arity match {
							case 2  => PrimitiveTypes.collTypes(sj.collCode)( (tv(0),tv(1)) )
							case 3  => PrimitiveTypes.collTypes(sj.collCode)( (tv(0),tv(1),tv(2)) )
							case 4  => PrimitiveTypes.collTypes(sj.collCode)( (tv(0),tv(1),tv(2),tv(3)) )
							case 5  => PrimitiveTypes.collTypes(sj.collCode)( (tv(0),tv(1),tv(2),tv(3),tv(4)) )
							case 6  => PrimitiveTypes.collTypes(sj.collCode)( (tv(0),tv(1),tv(2),tv(3),tv(4),tv(5)) )
							case 7  => PrimitiveTypes.collTypes(sj.collCode)( (tv(0),tv(1),tv(2),tv(3),tv(4),tv(5),tv(6)) )
							case 8  => PrimitiveTypes.collTypes(sj.collCode)( (tv(0),tv(1),tv(2),tv(3),tv(4),tv(5),tv(6),tv(7)) )
							case 9  => PrimitiveTypes.collTypes(sj.collCode)( (tv(0),tv(1),tv(2),tv(3),tv(4),tv(5),tv(6),tv(7),tv(8)) )
							case 10 => PrimitiveTypes.collTypes(sj.collCode)( (tv(0),tv(1),tv(2),tv(3),tv(4),tv(5),tv(6),tv(7),tv(8),tv(9)) )
							case 11 => PrimitiveTypes.collTypes(sj.collCode)( (tv(0),tv(1),tv(2),tv(3),tv(4),tv(5),tv(6),tv(7),tv(8),tv(9),tv(10)) )
							case 12 => PrimitiveTypes.collTypes(sj.collCode)( (tv(0),tv(1),tv(2),tv(3),tv(4),tv(5),tv(6),tv(7),tv(8),tv(9),tv(10),tv(11)) )
							case 13 => PrimitiveTypes.collTypes(sj.collCode)( (tv(0),tv(1),tv(2),tv(3),tv(4),tv(5),tv(6),tv(7),tv(8),tv(9),tv(10),tv(11),tv(12)) )
							case 14 => PrimitiveTypes.collTypes(sj.collCode)( (tv(0),tv(1),tv(2),tv(3),tv(4),tv(5),tv(6),tv(7),tv(8),tv(9),tv(10),tv(11),tv(12),tv(13)) )
							case 15 => PrimitiveTypes.collTypes(sj.collCode)( (tv(0),tv(1),tv(2),tv(3),tv(4),tv(5),tv(6),tv(7),tv(8),tv(9),tv(10),tv(11),tv(12),tv(13),tv(14)) )
							case 16 => PrimitiveTypes.collTypes(sj.collCode)( (tv(0),tv(1),tv(2),tv(3),tv(4),tv(5),tv(6),tv(7),tv(8),tv(9),tv(10),tv(11),tv(12),tv(13),tv(14),tv(15)) )
							case 17 => PrimitiveTypes.collTypes(sj.collCode)( (tv(0),tv(1),tv(2),tv(3),tv(4),tv(5),tv(6),tv(7),tv(8),tv(9),tv(10),tv(11),tv(12),tv(13),tv(14),tv(15),tv(16)) )
							case 18 => PrimitiveTypes.collTypes(sj.collCode)( (tv(0),tv(1),tv(2),tv(3),tv(4),tv(5),tv(6),tv(7),tv(8),tv(9),tv(10),tv(11),tv(12),tv(13),tv(14),tv(15),tv(16),tv(17)) )
							case 19 => PrimitiveTypes.collTypes(sj.collCode)( (tv(0),tv(1),tv(2),tv(3),tv(4),tv(5),tv(6),tv(7),tv(8),tv(9),tv(10),tv(11),tv(12),tv(13),tv(14),tv(15),tv(16),tv(17),tv(18)) )
							case 20 => PrimitiveTypes.collTypes(sj.collCode)( (tv(0),tv(1),tv(2),tv(3),tv(4),tv(5),tv(6),tv(7),tv(8),tv(9),tv(10),tv(11),tv(12),tv(13),tv(14),tv(15),tv(16),tv(17),tv(18),tv(19)) )
							case 21 => PrimitiveTypes.collTypes(sj.collCode)( (tv(0),tv(1),tv(2),tv(3),tv(4),tv(5),tv(6),tv(7),tv(8),tv(9),tv(10),tv(11),tv(12),tv(13),tv(14),tv(15),tv(16),tv(17),tv(18),tv(19),tv(20)) )
							case 22 => PrimitiveTypes.collTypes(sj.collCode)( (tv(0),tv(1),tv(2),tv(3),tv(4),tv(5),tv(6),tv(7),tv(8),tv(9),tv(10),tv(11),tv(12),tv(13),tv(14),tv(15),tv(16),tv(17),tv(18),tv(19),tv(20),tv(21)) )
						}
					} else {
						val listAcc = MList.empty[Any]
						if( idx.tokType(i) != JSlistStart ) 
							throw new JsonParseException(s"Expected JSlistStart and found ${JsonTokens.toName(idx.tokType(i))}",0)
						i += 1
						while( idx.tokType(i) != JSlistEnd && idx.tokType(i) != JSlistEndInList && idx.tokType(i) != JSlistEndObjKey ) {
							listAcc.append(_parse(sj.colTypes(0)))
						}
						PrimitiveTypes.collTypes(sj.collCode)(listAcc)
					}
				}
				i += 1
				ret

			// This is messed up (in Scala)...  If sj is a type parameter (of something else) then we wrap the value in the
			// value class, otherwise just supply the primitive value and let the JVM do the rest. <sigh>
			case sj:ValueClassType =>
				if( sj.isTypeParam || topLevel ) {
					parseValueClass(sj, parseValueClassPrimitive(sj)).asInstanceOf[T]
				} else {
					parseValueClassPrimitive(sj).asInstanceOf[T]
				}

			case sj:JavaType =>
				val handler = vctx.customHandlers.getOrElse(sj.name, throw new JsonParseException(s"No custom read/render handler (CustomReadRender) provided for class ${sj.name}",0))
				val java = handler.read.applyOrElse( (JsonKind(), idx.getToken(i)),
					(k:(KindMarker, _)) => throw new JsonParseException(s"No JSON read code provided in CustomReadRender handler for class ${sj.name}",0)
				)
				i += 1
				java
		}

		def parseValueClassPrimitive( vc:ValueClassType ) = 
			vc.custom.map{ _.read.applyOrElse(
					(JsonKind(), _parse(PrimType("String")).asInstanceOf[String] ),
					(k:(KindMarker, _)) => _parse(vc.vcType)
				)}.orElse( Some(_parse(vc.vcType)) ).get.asInstanceOf[AnyRef]
		def parseValueClass( vc:ValueClassType, primitive:AnyRef ) = Class.forName(vc.name).getConstructors()(0).newInstance(primitive)

		def findTypeHint( hint:String ) : Option[String] = {
			var thI = i
			var done = false
			val imax = idx.tokCount
			var retval : Option[String] = None
			while( !done ) {
				idx.tokType(thI) match {
					case JSobjStart => skipValue()
					case JSstringObjKey if(idx.getToken(thI) == hint) => 
						thI += 1
						retval = Some(idx.getToken(thI))
						done = true
					case _ => thI += 1
				}
				if( thI == idx.tokCount ) done = true
			}
			retval
		}

		def skipValue() : Unit = {
			idx.tokType(i) match {
				case JSlistStart | JSobjStart =>
					i += 1
					var done = false
					while( !done ) {
						idx.tokType(i) match {
							case JSlistStart | JSobjStart =>
								skipValue()
							case JSlistEnd | JSlistEndInList | JSobjEnd | JSobjEndInList => 
								i += 1
								done = true
							case _ =>
								i += 1
						}
					}
				case _ => i += 1  // simple, single value
			}
		}

		def inferSimpleType(idx:JsonIndex, start:Int):(Int,Any) = {
			var i = start
			idx.tokType(i) match {
				case JSlistStart => 
					i += 1
					val acc = new scala.collection.mutable.ListBuffer[Any]()
					while(idx.tokType(i) != JSlistEnd && idx.tokType(i) != JSlistEndInList) {
						val (newI,v) = inferSimpleType(idx,i)
						i = newI
						acc += v
					}
					(i+1,acc.toList)
				case JSobjStart => 
					i += 1
					val acc = new scala.collection.mutable.HashMap[String,Any]()
					while(idx.tokType(i) != JSobjEnd && idx.tokType(i) != JSobjEndInList) {
						val (newI,k) = inferSimpleType(idx,i)
						i = newI
						val (newI2,v) = inferSimpleType(idx,i)
						i = newI2
						acc += (k.toString -> v)
					}
					(i+1,acc.toMap)
				case JSstringObjKey | JSstring | JSstringInList =>
					(i+1,PrimitiveTypes.primTypes(PrimitiveTypes.STRING)( Unicode.unescape_perl_string(idx.getToken(i)) ))  // String
				case JSnumberObjKey | JSnumber | JSnumberInList =>
					val raw = Unicode.unescape_perl_string(idx.getToken(i))
					if( raw.contains('.') )
						(i+1,PrimitiveTypes.primTypes(PrimitiveTypes.DOUBLE)( raw ))  // scala.Double
					else
						(i+1,PrimitiveTypes.primTypes(PrimitiveTypes.INT)( raw ))  // scala.Int
				case JStrue | JStrueInList | JSfalse | JSfalseInList =>
					(i+1,PrimitiveTypes.primTypes(PrimitiveTypes.BOOLEAN)( Unicode.unescape_perl_string(idx.getToken(i)) )) // scala.Boolean
				case JSnull | JSnullInList =>
					(i+1,null)
				// case z => println("Boom: "+z)
				// (0,null)
			}
		}

		// Make it happen!
		_parse(Analyzer.inspectByName(sjTName),true).asInstanceOf[T]
	}
}

