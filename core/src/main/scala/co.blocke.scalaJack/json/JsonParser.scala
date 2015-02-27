package co.blocke.scalajack
package json

import scala.reflect.runtime.currentMirror
import PrimitiveTypes._
import scala.reflect.runtime.universe._
import JsonTokens._
import scala.collection.mutable.{Map => MMap,ListBuffer => MList}

/*
case class ParseFrame() {
	// var inObjVal  : Boolean         = false
	// var objKey    : Any             = null
	val objFields : MMap[Any,Any]   = MMap.empty[Any,Any]
	val listItems : ListBuffer[Any] = ListBuffer.empty[Any]
	// var item      : SjItem          = null
}
*/

/*
trait JsonParser {
	val s:Array[Char]
	val idx:JsonIndex
	def parse[T]()(implicit tt:TypeTag[T]) : T = null.asInstanceOf[T]


	/**
	 * Magically create an instance of a case class given a map of name->value parameters.
	 * (Reflects on the apply method of the case class' companion object.)
	 * It's a quick way to materialize a cse class represented by a Map, which is how ScalaJack uses it.
	 * ScalaJack parses the JSON, building a value Map as it goes.  When the JSON object has been parsed
	 * ScalaJack calls poof to build the case class from the Map.
	 */
	private[json] def poof( cc:SjCaseClass, data:Map[String,Any] ) : Any = {
		val args = cc.fields.collect{ case f => data.get(f.fieldName).getOrElse(None) }.toArray.asInstanceOf[Array[AnyRef]]
		cc.constructor.apply( args:_* )
	}
}

// 2 Options: 
//  1) Visit the SjTypes
//  2) Visit the JSON tokens
//
// Choice: #2.  We can't control the token order so assemble these first as we find them in the JSON,
//    then map against the expected type represented by SjType
*/

case class JsonParser(sjTName:String, s:Array[Char], idx:JsonIndex, typeHint:String) {

	/**
	 * Magically create an instance of a case class given a map of name->value parameters.
	 * (Reflects on the apply method of the case class' companion object.)
	 * It's a quick way to materialize a cse class represented by a Map, which is how ScalaJack uses it.
	 * ScalaJack parses the JSON, building a value Map as it goes.  When the JSON object has been parsed
	 * ScalaJack calls poof to build the case class from the Map.
	 */
	private def poof( cc:SjCaseClass, data:Map[String,Any] ) : Any = {
		val args = cc.fields.collect{ case f => data.get(f.fieldName).getOrElse(None) }.toArray.asInstanceOf[Array[AnyRef]]
		Class.forName(cc.name).getConstructors()(0).newInstance(args:_*)
		// cc.constructor.apply( args:_* )   -- didn't seem to work for value classes, but newInstance did... hmm...
	}

	// This is a variant of the version of getGraph in ReadRenderFrame.  This one cooks the graph by class name *and* has 
	// different implicit parameters.
	private def getGraph2(className:String)(implicit t:Type) = {
		val csym = currentMirror.classSymbol(Class.forName(className))
		if( csym.isCollection ) { 
			// handle naked collections -- kinda ugly
			val naked = Analyzer.nakedInspect(t.typeArgs)
			SjCollection(PrimitiveTypes.fixPolyCollection(csym.fullName).get,naked)
		} else
			Analyzer.inspect(className) // normal non-collection case
	}

	def parse[T]()(implicit tt:TypeTag[T]) : T = {
		var i = 0  // index into idx

		def _makeClass( ccTypeFn : ()=>SjCaseClass, t:SjType ) = {
			val objFields = MMap.empty[Any,Any]
			if( idx.tokType(i) != JSobjStart ) throw new JsonParseException(s"Expected JSobjStart and found ${JsonTokens.toName(idx.tokType(i))}",0)
			i += 1

			val sjT = ccTypeFn()
			// Map any class type parameters
			val paramTypes = tt.tpe.typeArgs.map(ttype => {
				implicit val x = ttype
				getGraph2(ttype.typeSymbol.fullName)
				})
			val tparms = sjT.params.zip(paramTypes).toMap

			// read key/value pairs
	println("OBJ: "+sjT)
	println("Pms: "+tparms)
			while( idx.tokType(i) != JSobjEnd && idx.tokType(i) != JSobjEndInList  && idx.tokType(i) != JSobjEndObjKey) {
				if( idx.tokType(i) != JSstringObjKey ) throw new JsonParseException(s"Expected JSstringObjKey and found ${JsonTokens.toName(idx.tokType(i))}",0)
				val fieldName = idx.getToken(i,s)
				i += 1
				println("Finding field: "+fieldName)
				sjT.fields.find(_.fieldName == fieldName).fold( skipValue )( _.ftype match {
					case vt:SjValueClass =>
						// Voodoo here--if a value class is a property of a case class, unwrap it--don't create the value class object
						objFields.put(fieldName, _parse(vt.vcType, tparms))
					case vt:SjTypeSymbol =>
						objFields.put(fieldName, _parse(tparms(vt.name), tparms))
println("FIELD: "+fieldName)
					case vt =>
						objFields.put(fieldName, _parse(vt, tparms))
				})
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
			poof( sjT, objFields.toMap.asInstanceOf[Map[String,Any]] )
		}

		def _parse( t:SjType, params:Map[String,SjType] = Map.empty[String,SjType] ) : Any = t match {
			case sj:SjCaseClass =>
				_makeClass( ()=>{sj}, t )

			case sj:SjTrait =>
				_makeClass( ()=>{
					// Look-ahead and find type hint--figure out what kind of object his is and inspect it.
					val objClass = findTypeHint(typeHint).getOrElse(typeHint, throw new JsonParseException(s"No type hint $typeHint given for trait ${sj.name}",0))
					val sjObjType = Analyzer.inspect(objClass.toString)
					if( !sjObjType.isInstanceOf[SjCaseClass] ) throw new JsonParseException(s"Type hint $objClass does not specify a case class",0)
					sjObjType.asInstanceOf[SjCaseClass]
					}, t)

			case sj:SjPrimitive =>
				val v = idx.tokType(i) match {
					case JSstringObjKey | JSstring | JSstringInList | JSnumberObjKey | JSnumber | JSnumberInList =>
						PrimitiveTypes.primitiveTypes(sj.name)(idx.getToken(i,s))
					case JStrue  => true
					case JSfalse => false
					case JSnull  => null
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

			case sj:SjValueClass =>
  				val primitive = _parse(sj.vcType).asInstanceOf[AnyRef]
				Class.forName(sj.name).getConstructors()(0).newInstance(primitive)
		}

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
		implicit val x = tt.tpe
		_parse(getGraph2(sjTName)).asInstanceOf[T]
	}
}

