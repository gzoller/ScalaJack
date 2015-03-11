package co.blocke.scalajack
package json

import scala.reflect.runtime.currentMirror
import PrimitiveTypes._
import scala.reflect.runtime.universe._
import JsonTokens._
import scala.collection.mutable.{Map => MMap,ListBuffer => MList}

case class JsonParser(sjTName:String, s:Array[Char], idx:JsonIndex, typeHint:String) {

	/**
	 * Magically create an instance of a case class given a map of name->value parameters.
	 * (Reflects on the apply method of the case class' companion object.)
	 * It's a quick way to materialize a cse class represented by a Map, which is how ScalaJack uses it.
	 * ScalaJack parses the JSON, building a value Map as it goes.  When the JSON object has been parsed
	 * ScalaJack calls poof to build the case class from the Map.
	 */
	private def poof[T]( cc:SjCaseClass, data:Map[String,Any] )(implicit tt:TypeTag[T]) : Any = {
		// Get constructor arguments in right order, we should.
		val args = cc.fields.collect{ case f => data.get(f.fieldName).getOrElse(None) }.toArray.asInstanceOf[Array[AnyRef]]
		Class.forName(cc.name).getConstructors()(0).newInstance(args:_*)

		// Can go to this one after Scala fixes their bug: https://issues.scala-lang.org/browse/SI-9102
		// At that point we'll compute ctor differently (see Analyzer.scala).  We also won't need all the special handling
		// of SjValueClass objects in _parse in this file.
		// cc.ctor.apply( args: _* )   //-- didn't seem to work for value classes, but newInstance did... hmm...
	}

	// This is a variant of the version of getGraph in ReadRenderFrame.  This one cooks the graph by class name *and* has 
	// different implicit parameters.
	private def getGraph2[T](className:String)(implicit t:TypeTag[T]) = {
		val csym = currentMirror.classSymbol(Class.forName(className))
		if( csym.isCollection ) { 
			// handle naked collections -- kinda ugly
			val naked = Analyzer.nakedInspect(t.tpe.typeArgs)
			SjCollection(PrimitiveTypes.fixPolyCollection(csym.fullName).get,naked)
		} else
			Analyzer.inspectByName(className) // normal non-collection case
	}

	def parse[T]()(implicit tt:TypeTag[T]) : T = {
		var i = 0  // index into idx

		def _makeClass[U]( ccTypeFn : ()=>SjCaseClass, t:SjType )(implicit ty:TypeTag[U]) = {
			val objFields = MMap.empty[Any,Any]
			if( idx.tokType(i) != JSobjStart ) throw new JsonParseException(s"Expected JSobjStart and found ${JsonTokens.toName(idx.tokType(i))} at token $i",0)
			i += 1

			val sjT = ccTypeFn()

			// read key/value pairs
			while( idx.tokType(i) != JSobjEnd && idx.tokType(i) != JSobjEndInList  && idx.tokType(i) != JSobjEndObjKey) {
				if( idx.tokType(i) != JSstringObjKey ) throw new JsonParseException(s"Expected JSstringObjKey and found ${JsonTokens.toName(idx.tokType(i))}",0)
				val fieldName = idx.getToken(i,s)
				i += 1
				sjT.fields.find(_.fieldName == fieldName).fold( skipValue )( f => objFields.put(fieldName, _parse(f.ftype)) )
			}
			i+=1

			// Ensure all needed class fields are there, setting aside any missing Optional fields (which become None)
			// val missing = sj.fields.map(_.fieldName).toSet.diff(objFields.keySet)
			val fnames = objFields.keySet
			val missing = sjT.fields.collect{
				case f if(fnames.contains(f.fieldName)) => 
					None  // field found -- don't collect
				case f if(f.ftype.isInstanceOf[SjCollection] && f.ftype.asInstanceOf[SjCollection].isOptional) =>
				 	// Missing but optional => None  -- don't collect
					objFields.put(f.fieldName,None)
					None
				case f => 
					Some(f)
				}.flatten.map(_.fieldName)

			if(missing.size > 0) throw new JsonParseException(s"""No values parsed for field(s) ${missing.mkString(",")} for class ${t.name}""",0)
			poof( sjT, objFields.toMap.asInstanceOf[Map[String,Any]] )(ty)
		}

		// def _parse( t:SjType, params:Map[String,SjType] = Map.empty[String,SjType] ) : Any = t match {
		def _parse( t:SjType, topLevel:Boolean = false ) : Any = t match {
			case sj:SjCaseClass =>
				_makeClass( ()=>{sj}, t )

			case sj:SjTrait =>
				_makeClass( ()=>{
					// Look-ahead and find type hint--figure out what kind of object his is and inspect it.
					val objClass = findTypeHint(typeHint).getOrElse(typeHint, throw new JsonParseException(s"No type hint $typeHint given for trait ${sj.name}",0))
					val sjObjType = Analyzer.inspectByName(objClass.toString,Some(sj))
					if( !sjObjType.isInstanceOf[SjCaseClass] ) throw new JsonParseException(s"Type hint $objClass does not specify a case class",0)
					sjObjType.asInstanceOf[SjCaseClass]
					}, t)

			case sj:SjPrimitive =>
				val v = idx.tokType(i) match {
					case JSstringObjKey | JSstring | JSstringInList | JSnumberObjKey | JSnumber | JSnumberInList =>
						PrimitiveTypes.primitiveTypes(sj.name)( Unicode.unescape_perl_string(idx.getToken(i,s)) )
					case JStrue  | JStrueInList  => true
					case JSfalse | JSfalseInList => false
					case JSnull  | JSnullInList  => null
					case _ => throw new JsonParseException(s"Expected primitive value but saw ${JsonTokens.toName(idx.tokType(i))} obj ${idx.getToken(i,s)}",0)
				}
				i += 1
				v

			case sj:SjEnum =>
				try {
					sj.enum.withName( idx.getToken(i,s) )
				} catch {
					case w:Throwable => throw new JsonParseException(s"Value ${idx.getToken(i,s)} is not a valid for enum ${sj.enum.toString}",0)
				} finally {
					i += 1
				}

			case sj:SjCollection =>
				val ret = { 
					if( sj.isOptional ) {
						val parsed = _parse(sj.collectionType(0))
						i -= 1  // compensate for increment later
						Some(parsed)
					} else if(sj.name.endsWith("Map")) {
						val mapAcc = MMap.empty[Any,Any]
						if( idx.tokType(i) != JSobjStart ) 
							throw new JsonParseException(s"Expected JSlistStart and found ${JsonTokens.toName(idx.tokType(i))}",0)
						i += 1
						while( idx.tokType(i) != JSobjEnd && idx.tokType(i) != JSobjEndInList ) {
							val key = _parse(sj.collectionType(0))
							val value = _parse(sj.collectionType(1)) 
							mapAcc.put(key,value)
						}
						PrimitiveTypes.scalaCollections(sj.name)(mapAcc.toList)
					} else {
						val listAcc = MList.empty[Any]
						if( idx.tokType(i) != JSlistStart ) 
							throw new JsonParseException(s"Expected JSlistStart and found ${JsonTokens.toName(idx.tokType(i))}",0)
						i += 1
						while( idx.tokType(i) != JSlistEnd && idx.tokType(i) != JSlistEndInList ) {
							listAcc.append(_parse(sj.collectionType(0)))
						}
						PrimitiveTypes.scalaCollections(sj.name)(listAcc)
					}
				}
				i += 1
				ret

			// This is messed up (in Scala)...  If sj is a type parameter (of something else) then we wrap the value in the
			// value class, otherwise just supply the primitive value and let the JVM do the rest. <sigh>
			case sj:SjValueClass =>
				if( sj.isTypeParam || topLevel ) {
					parseValueClass(sj, parseValueClassPrimitive(sj)).asInstanceOf[T]
				} else {
					parseValueClassPrimitive(sj)
				}
		}

		def parseValueClassPrimitive( vc:SjValueClass ) =  _parse(vc.vcType).asInstanceOf[AnyRef]
		def parseValueClass( vc:SjValueClass, primitive:AnyRef ) = Class.forName(vc.name).getConstructors()(0).newInstance(primitive)

		def findTypeHint( hint:String ) : Option[String] = {
			var saveI = i
			var done = false
			val imax = idx.tokCount
			var retval : Option[String] = None
			while( !done ) {
				idx.tokType(i) match {
					case JSobjStart => skipValue()
					case JSstringObjKey if(idx.getToken(i,s) == hint) => 
						i += 1
						retval = Some(idx.getToken(i,s))
						done = true
					case _ => i += 1
				}
				if( i == idx.tokCount ) done = true
			}
			i = saveI
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

		// Make it happen!
		val z = getGraph2(sjTName)
		// println("IDX: "+this.idx.tokType.slice(0,8).toList)
		// println("Z: "+z)
		_parse(z,true).asInstanceOf[T]
		
		// _parse(getGraph2(sjTName)).asInstanceOf[T]
	}
}

