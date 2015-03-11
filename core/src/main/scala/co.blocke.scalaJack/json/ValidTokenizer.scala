package co.blocke.scalajack
package json

import JsonTokens._
import scala.collection.mutable.ArrayBuffer

/**
 * ValidTokenizer is a fair bit slower than FastTokenizer, but... you get full 
 * JSON validaton.  You also don't need to worry about estimating element capacity
 * as this uses dynamic array buffers.  ValidTokenizer will give you high-quality
 * information if the JSON is wrong, so it's ideal for JSON you aren't confident about.
 *
 * If your code is _really_ sophisticated, maybe it can first try to use the FastTokenizer,
 * and catch failures in a try block.  The resolution of the error might be to re-parse
 * the troublesome JSON with the ValidTokenizer for a decent error message.  Hmm...
 *
 * As with all the tokenizers, this is *NOT* thread-safe!  Don't share these across threads!
 */

case class ValidTokenizer( isCanonical:Boolean = true ) extends JsonTokenizer {
	// Context stack - tracks object/list depth
	private val CTX_OBJ  : Byte = 1
	private val CTX_LIST : Byte = 2
	private val ctxStack = new Array[Byte](256)
	private var ctxPtr   = -1

	// Token buffers
	private val tokPos  = ArrayBuffer.empty[Int]
	private val tokLen  = ArrayBuffer.empty[Int]
	private val tokType = ArrayBuffer.empty[Byte]
  
	def tokenize( s:Array[Char] ) = {
		val sLen = s.length
		var i    = 0
		ctxPtr   = -1
		tokPos.clear()
		tokLen.clear()
		tokType.clear()

		var lastToken1 : Byte = 0
		var lastPos1   : Int  = 0
		var lastToken2 : Byte = 0
		var lastPos2   : Int  = 0
    
		while( i < sLen ) {
			s(i) match {
				case ' ' | '\n' | '\t' => // skip whitespace

				case '"' =>
					i += 1
					tokPos += i
					var strStart = i
					var skip = false
					while( s(i)!='"' || skip ) { 
						if(s(i) == '\\' )
							skip = true
						else
							skip = false
						i += 1
					}
					tokLen  += i - strStart
					tokType += JSstring
					JsonValidator.validate(lastPos2,lastToken1,lastToken2,isCanonical)
					lastToken1 = lastToken2
					lastPos1   = lastPos2
					lastToken2 = JSstring
					lastPos2   = strStart-1  // (-1 to account for " char)

				case '{' => 
					ctxPtr += 1  // stack push
					ctxStack(ctxPtr) = CTX_OBJ
					tokPos  += i
					tokLen  += 1
					tokType += JSobjStart

					JsonValidator.validate(lastPos2,lastToken1,lastToken2,isCanonical)
					lastToken1 = lastToken2
					lastPos1   = lastPos2
					lastToken2 = JSobjStart
					lastPos2   = i

				case '}' =>
					ctxPtr -= 1  // stack pop
					tokPos  += i
					tokLen  += 1
					tokType += JSobjEnd

					JsonValidator.validate(lastPos2,lastToken1,lastToken2,isCanonical)
					lastToken1 = lastToken2
					lastPos1   = lastPos2
					lastToken2 = JSobjEnd
					lastPos2   = i

				case '[' =>
					ctxPtr += 1  // stack push
					ctxStack(ctxPtr) = CTX_LIST
					tokPos  += i
					tokLen  += 1
					tokType += JSlistStart

					JsonValidator.validate(lastPos2,lastToken1,lastToken2,isCanonical)
					lastToken1 = lastToken2
					lastPos1   = lastPos2
					lastToken2 = JSlistStart
					lastPos2   = i

				case ']' =>
					ctxPtr -= 1  // stack pop
					// Adjust last element in list to list variant
					if( tokType(tokType.length-1) != JSlistStart )
						tokType.update(tokType.length-1, (tokType(tokType.length-1) << 1).toByte)
					lastToken2 = (lastToken2 << 1).toByte
					tokPos  += i
					tokLen  += 1
					tokType += JSlistEnd

					JsonValidator.validate(lastPos2,lastToken1,lastToken2,isCanonical)
					lastToken1 = lastToken2
					lastPos1   = lastPos2
					lastToken2 = JSlistEnd
					lastPos2   = i

				case ',' =>
					// Convert last token to a list variant if we're in list context
					val toBeToken =  {
						if(ctxStack(ctxPtr) == CTX_LIST ) {
							tokType.update(tokType.length-1, (tokType(tokType.length-1) << 1).toByte)
							lastToken2 = (lastToken2 << 1).toByte
							JScommaInList
						} else 
							JScomma
					}

					JsonValidator.validate(lastPos2,lastToken1,lastToken2,isCanonical)
					lastToken1 = lastToken2
					lastPos1   = lastPos2
					lastToken2 = toBeToken
					lastPos2   = i

				case ':' => 
					// if( tokType(tokType.length-1) != JSstring && isCanonical ) 
					// 	throw new JsonParseException(s"JSON parse error.  Non-String object keys not supported in canonical JSON at position ${tokPos(tokType.length-1)}",tokPos(tokType.length-1))
					tokType.update(tokType.length-1, (tokType(tokType.length-1) << 2).toByte)
					lastToken2 = (lastToken2 << 2).toByte
					// Note: not added to index!

					JsonValidator.validate(lastPos2,lastToken1,lastToken2,isCanonical)
					lastToken1 = lastToken2
					lastPos1   = lastPos2
					lastToken2 = JScolon
					lastPos2   = i

				case 't' =>
					tokPos  += i
					tokLen  += 4
					tokType += JStrue
					i += 3

					JsonValidator.validate(lastPos2,lastToken1,lastToken2,isCanonical)
					lastToken1 = lastToken2
					lastPos1   = lastPos2
					lastToken2 = JStrue
					lastPos2   = i

				case 'f' =>
					tokPos  += i
					tokLen  += 5
					tokType += JSfalse
					i += 4

					JsonValidator.validate(lastPos2,lastToken1,lastToken2,isCanonical)
					lastToken1 = lastToken2
					lastPos1   = lastPos2
					lastToken2 = JSfalse
					lastPos2   = i

				case 'n' =>
					tokPos  += i
					tokLen  += 4
					tokType += JSnull
					i += 3

					JsonValidator.validate(lastPos2,lastToken1,lastToken2,isCanonical)
					lastToken1 = lastToken2
					lastPos1   = lastPos2
					lastToken2 = JSnull
					lastPos2   = i

				case c if(c.isDigit || c == '-') =>
					tokPos  += i
					var numStart = i
					while( s(i).isDigit || s(i)=='e' || s(i)=='E' || s(i)=='.' || s(i)=='+' || s(i)=='-' ) 
						i += 1
					tokLen  += i - numStart
					tokType += JSnumber
					i -= 1 // back up one... process non-number char

					JsonValidator.validate(lastPos2,lastToken1,lastToken2,isCanonical)
					lastToken1 = lastToken2
					lastPos1   = lastPos2
					lastToken2 = JSnumber
					lastPos2   = numStart

				case c => 
					throw new JsonParseException(s"Character out of place (bad JSON) at position $i",i)
			}
			i += 1
		}
	// One last validation
	JsonValidator.validate(lastPos2,lastToken1,lastToken2,isCanonical)
	if( ctxPtr >= 0 ) throw new JsonParseException("Incomplete (open) object or list in JSON.  Forgot closing } or ]?",i,true)

	JsonIndex(tokPos.size,tokPos.toArray,tokLen.toArray,tokType.toArray)
	}
}