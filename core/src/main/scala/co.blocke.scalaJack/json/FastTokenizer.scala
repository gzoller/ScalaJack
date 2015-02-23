package co.blocke.scalajack
package json

import JsonTokens._

/**
 * FastTokenizer is the fastest way to tokenize JSON in ScalaJack.  It's fast.
 * It gets its speed, in part, by assuming the given JSON is correct, so there's
 * almost nothing in the way of checking.  If it does fail, it blows up with
 * exceptions--not all of them helpful.
 *
 * Note there's a lot of "primitive" code here, e.g. avoidance of data structures/classes
 * in favor of basic Arrays, etc.  This is for speed.
 *
 * FastTokenizer is best used when you know your JSON is reliable and correct.
 *
 * The capacity parameter requires some guess about the JSON data you have.  It is the
 * size of the arrays created to hold all the index (element) data.  It should be 
 * safely larger than the maximum number of token elements you reasonably expect to see
 * in a JSON string.  For example "[1,2,3]" has 7 elements, while 
 * "{"one":1239,"two":false, "three":"hey"} as 8.  If you have a weak stomach it can
 * be equal to the size of the largest JSON string, but that's probably wild overkill.
 *
 * As with all the tokenizers, this is *NOT* thread-safe!  Don't share these across threads!
 */

case class FastTokenizer( capacity:Int ) extends JsonTokenizer {
	// Context stack - tracks object/list depth
	private val CTX_OBJ  : Byte = 1
	private val CTX_LIST : Byte = 2
	private val ctxStack = new Array[Byte](256)
	private var ctxPtr   = -1

	// Token buffers
	private val tokPos  = new Array[Int](capacity)
	private val tokLen  = new Array[Int](capacity)
	private val tokType = new Array[Byte](capacity)
	private var tokPtr  = 0
  
	def tokenize( s:Array[Char] ) = {
		val sLen = s.length
		var i    = 0
		ctxPtr   = -1
		tokPtr   = 0
    
		while( i < sLen ) {
			s(i) match {
				case ' ' | '\n' | '\t' => // skip whitespace

				case '"' =>
					i += 1
					tokPos(tokPtr) = i
					var strStart = i
					var skip = false
					while( s(i)!='"' || skip ) { 
						if(s(i) == '\\' )
							skip = true
						else
							skip = false
						i += 1
					}
					tokLen(tokPtr)  = i - strStart
					tokType(tokPtr) = JSstring
					tokPtr += 1

				case '{' => 
					ctxPtr += 1  // stack push
					ctxStack(ctxPtr) = CTX_OBJ
					tokPos(tokPtr) = i
					tokLen(tokPtr) = 1
					tokType(tokPtr) = JSobjStart
					tokPtr += 1

				case '}' =>
					ctxPtr -= 1  // stack pop
					tokPos(tokPtr) = i
					tokLen(tokPtr) = 1
					tokType(tokPtr) = JSobjEnd
					tokPtr += 1

				case '[' =>
					ctxPtr += 1  // stack push
					ctxStack(ctxPtr) = CTX_LIST
					tokPos(tokPtr) = i
					tokLen(tokPtr) = 1
					tokType(tokPtr) = JSlistStart
					tokPtr += 1

				case ']' =>
					ctxPtr -= 1  // stack pop
					// Adjust last element in list to list variant
					tokType(tokPtr-1) = (tokType(tokPtr-1) << 1).toByte 
					tokPos(tokPtr) = i
					tokLen(tokPtr) = 1
					tokType(tokPtr) = JSlistEnd
					tokPtr += 1

				case ',' =>
					// Convert last token to a list variant if we're in list context
					if(ctxStack(ctxPtr) == CTX_LIST ) tokType(tokPtr-1) = (tokType(tokPtr-1) << 1).toByte 

				case ':' => 
					tokType(tokPtr-1) = (tokType(tokPtr-1) << 2).toByte

				case 't' =>
					tokPos(tokPtr) = i
					tokLen(tokPtr) = 1
					tokType(tokPtr) = JStrue
					tokPtr += 1
					i += 3

				case 'f' =>
					tokPos(tokPtr) = i
					tokLen(tokPtr) = 1
					tokType(tokPtr) = JSfalse
					tokPtr += 1
					i += 4

				case 'n' =>
					tokPos(tokPtr) = i
					tokLen(tokPtr) = 1
					tokType(tokPtr) = JSnull
					tokPtr += 1
					i += 3

				case c if(c.isDigit || c == '-') =>
					tokPos(tokPtr) = i
					var numStart = i
					while( s(i)!=']' && s(i)!='}' && s(i)!=',' ) 
						i += 1
					tokLen(tokPtr) = i - numStart
					tokType(tokPtr) = JSnumber
					tokPtr += 1
					i -= 1 // back up one... process non-number char

				case c => throw new JsonParseException(s"Character out of place (bad JSON) at position $i",i)
			}
			i += 1
		}
	if( ctxPtr >= 0 ) throw new JsonParseException("Incomplete (open) object or list in JSON.  Forgot closing } or ]?",i,true)
	JsonIndex(tokPtr,tokPos,tokLen,tokType)
	}
}