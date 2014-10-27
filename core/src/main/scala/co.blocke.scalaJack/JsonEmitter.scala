package co.blocke.scalajack

import com.fasterxml.jackson.core._
import com.fasterxml.jackson.core.util.JsonParserDelegate
import ScalaJack.JSON

case class JsonEmitter(js:JSON) {

	private val jp = (new JsonFactory()).createParser(js)
	private val cache = scala.collection.mutable.Queue.empty[TokenRec]

	var cur = TokenRec(null,null,null)
	var savedState : Option[TokenRec] = None

	def cached = cache.size

	def peekNextToken() = {
		if( savedState.isEmpty ) 
			savedState = Some(TokenRec(
				jp.getCurrentToken, 
				jp.getCurrentName, 
				jp.getValueAsString))

		val nt = jp.nextToken
		val t = TokenRec(
			jp.getCurrentToken, 
			jp.getCurrentName, 
			jp.getValueAsString)
		cur = t
		cache.enqueue( t )
		nt
	}

	def restoreState() = 
		savedState.map( ss => {
			cur = ss
			savedState = None
			}) 

	def nextToken() : JsonToken = 
		if( cache.size > 0 ) {
			cur = cache.dequeue()
			cur.t
		} else 
			jp.nextToken

	def getCurrentToken() : JsonToken = 
		if( cache.size > 0 )
			cur.t
		else
			jp.getCurrentToken

	def getCurrentName() : String = 
		if( cache.size > 0 )
			cur.name
		else
			jp.getCurrentName

	def skipChildren( peek:Boolean = false ) = 
		getCurrentToken match {
			case JsonToken.START_OBJECT | JsonToken.START_ARRAY => 
				var open = 1
				/* Since proper matching of start/end markers is handled
				 * by nextToken(), we'll just count nesting levels here
				 */
				var isset = false
				while (!isset) {
					val t = { if(peek) peekNextToken else nextToken() }
					if (t == null) {
						isset = true
					} else if (t.isStructStart()) {
						open = open + 1
					} else if (t.isStructEnd()) {
						open = open - 1
						if (open == 0) 
							isset = true
					}
				}
			case _ =>
		}

	def getValueAsString()  : String  = if( cache.size > 0 ) cur.value           else jp.getValueAsString
	def getValueAsInt()     : Int     = if( cache.size > 0 ) cur.value.toInt     else jp.getValueAsInt
	def getValueAsLong()    : Long    = if( cache.size > 0 ) cur.value.toLong    else jp.getValueAsLong
	def getValueAsBoolean() : Boolean = if( cache.size > 0 ) cur.value.toBoolean else jp.getValueAsBoolean
	def getValueAsDouble()  : Double  = if( cache.size > 0 ) cur.value.toDouble  else jp.getValueAsDouble
}

case class TokenRec(
	t       : JsonToken,
	name    : String,
	value   : String
	)