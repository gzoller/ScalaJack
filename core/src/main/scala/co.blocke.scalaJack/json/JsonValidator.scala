package co.blocke.scalajack
package json

import JsonTokens._

object JsonValidator {
	// JSON "language" syntax tree--from node <key>, where are the valid next-nodes?
	private val validCanonical = Map( 
		JSobjStart      -> List(JSstringObjKey,JSobjEnd),  // JSstring -> JSstringObjKey validation will happy on ':'
		JSobjEnd        -> List(JScomma,JSobjEnd,JSobjEndInList),
		JSobjEndInList  -> List(JScommaInList,JSlistEnd,JSlistEndInList),
		JScomma         -> List(JSstringObjKey),  // JSstring -> JSstringObjKey validation will happy on ':'
		JScommaInList   -> List(JSobjStart,JSlistStart,JStrueInList,JSfalseInList,JSnullInList,JSstringInList,JSnumberInList),
		JScolon         -> List(JSobjStart,JSlistStart,JStrue,JSfalse,JSnull,JSstring,JSnumber),
		JSlistStart     -> List(JSlistEnd,JSlistEndInList,JSobjStart,JSlistStart,JStrueInList,JSfalseInList,JSnullInList,JSstringInList,JSnumberInList),
		JSlistEnd       -> List(JScomma,JSobjEnd,JSobjEndInList,JSlistEnd,JSlistEndInList),
		JSlistEndInList -> List(JScommaInList,JSlistEnd,JSlistEndInList),
		JStrue          -> List(JScomma,JSobjEnd,JSobjEndInList),
		JStrueInList    -> List(JScommaInList,JSlistEnd,JSlistEndInList),
		JSfalse         -> List(JScomma,JSobjEnd,JSobjEndInList),
		JSfalseInList   -> List(JScommaInList,JSlistEnd,JSlistEndInList),
		JSnull          -> List(JScomma,JSobjEnd,JSobjEndInList),
		JSnullInList    -> List(JScommaInList,JSlistEnd,JSlistEndInList),
		JSstring        -> List(JScomma,JSobjEnd,JSobjEndInList),
		JSstringInList  -> List(JScommaInList,JSlistEnd,JSlistEndInList),
		JSstringObjKey  -> List(JScolon),
		JSnumber        -> List(JScomma,JSobjEnd,JSobjEndInList),
		JSnumberInList  -> List(JScommaInList,JSlistEnd,JSlistEndInList)
		)

	private val validNonCanonical = validCanonical ++ Map( 
		JSobjStart      -> List(JSstringObjKey,JSlistEndObjKey,JSnumberObjKey,JSobjStart,JSobjEnd),
		JSobjEndObjKey  -> List(JScolon),
		JScomma         -> List(JSstringObjKey,JSobjStart,JSnumberObjKey),  // ObjKey validation will happy on ':'
		JSstring        -> List(JScomma,JSobjEnd,JSobjEndInList,JSobjEndObjKey),
		JStrue          -> List(JScomma,JSobjEnd,JSobjEndInList,JSobjEndObjKey),
		JSfalse         -> List(JScomma,JSobjEnd,JSobjEndInList,JSobjEndObjKey),
		JSnull          -> List(JScomma,JSobjEnd,JSobjEndInList,JSobjEndObjKey),
		JSobjEnd        -> List(JScomma,JSobjEnd,JSobjEndInList,JSobjEndObjKey),
		JSlistEnd       -> List(JScomma,JSobjEnd,JSobjEndInList,JSlistEnd,JSlistEndInList,JSobjEndObjKey),
		JSnumber        -> List(JScomma,JSobjEnd,JSobjEndInList,JSobjEndObjKey),
		JSlistEndObjKey -> List(JScolon),
		JSstringObjKey  -> List(JScolon),
		JSnumberObjKey  -> List(JScolon)
		)
 
	def validate( pos:Int, lastToken:Byte, thisToken:Byte, isCanonical:Boolean ) = { 
		// if( lastToken > 0 && thisToken > 0)
		// 	println(s"Validate: $pos ${JsonTokens.toName(lastToken)} -> ${JsonTokens.toName(thisToken)}")
		val parseMap = {
			if( isCanonical ) validCanonical
			else validNonCanonical
		}
		if(lastToken > 0 && thisToken > 0 && !parseMap(lastToken).contains(thisToken))
			throw new JsonParseException(s"JSON parse error.  Expected one of ${parseMap(lastToken).map(JsonTokens.toName(_)).mkString(" ")} but saw ${JsonTokens.toName(thisToken)} at position $pos",pos)
	}
}
