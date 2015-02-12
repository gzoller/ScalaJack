package co.blocke.scalajack
package test

import org.scalatest.{ FunSpec, GivenWhenThen, BeforeAndAfterAll }
import org.scalatest.Matchers._
import com.fasterxml.jackson.core._

class JsonCacheTests extends FunSpec with GivenWhenThen with BeforeAndAfterAll {

	val js = """{
		"name":"Greg",
		"age":35,
		"hobbies":["a","b","c"],
		"key":"something",
		"hold":{
			"one":"two",
			"three":"four"
			}
		}"""
	var currents = ""

	override def beforeAll() {
		val c1 = scala.collection.mutable.Queue.empty[String]
		val e = (new JsonFactory()).createParser(js)	
		var done = false
		while( !done ) {
			val t  = e.nextToken
			if( t == null ) 
				done = true
			else c1.enqueue({
				val x = e.getCurrentToken
				if( x == null ) "null" else x.toString
				})
		}
		currents = c1.toList.mkString(",")
	}

	describe("==========================\n| -- JSON Cache Tests -- |\n==========================") {
		
		it("Basic Queing/Dequeing") {
			val c1 = scala.collection.mutable.Queue.empty[String]
			val c2 = scala.collection.mutable.Queue.empty[String]
			val e = JsonEmitter(js)
			var done = false
			while( !done ) {
				val t = e.peekNextToken
				if( t == null )
					done = true
				else
					c1.enqueue( t.toString )
			}
			done = false
			while( !done ) {
				val t = e.nextToken
				if( t == null )
					done = true
				else
					c2.enqueue( t.toString )
			}
			e.cached should be( 0 )
			c1.toList.mkString(",") should equal( c2.toList.mkString(",") )
		}
		it("Partial Queing/Dequing") {
			val c1 = scala.collection.mutable.Queue.empty[String]
			val e = JsonEmitter(js)
			var i = 0
			var done = false
			while( !done ) {
				val t = {
					if( i < 10 ) {
						i = i+1
						e.peekNextToken
					} else e.nextToken
				}
				if( t == null )
					done = true
				else
					c1.enqueue( t.toString )
			}
			c1.toList should equal( List("START_OBJECT", "FIELD_NAME", "VALUE_STRING", "FIELD_NAME", "VALUE_NUMBER_INT", "FIELD_NAME", 
				"START_ARRAY", "VALUE_STRING", "VALUE_STRING", "VALUE_STRING", "START_OBJECT", "FIELD_NAME", "VALUE_STRING", "FIELD_NAME", 
				"VALUE_NUMBER_INT", "FIELD_NAME", "START_ARRAY", "VALUE_STRING", "VALUE_STRING", "VALUE_STRING", "END_ARRAY", "FIELD_NAME", 
				"VALUE_STRING", "FIELD_NAME", "START_OBJECT", "FIELD_NAME", "VALUE_STRING", "FIELD_NAME", "VALUE_STRING", "END_OBJECT", 
				"END_OBJECT") )
		}
		it("Current Token feature should work with and without cache") {
			val c2 = scala.collection.mutable.Queue.empty[String]
			val e2 = JsonEmitter(js)
			var i = 0
			var done = false
			while( !done ) {
				if( i == 10 ) e2.restoreState
				if( i < 10 ) {
					i = i+1
					e2.peekNextToken
				} else {
					val t = e2.nextToken
					if( t == null )
						done = true
					else c2.enqueue( {
						val x = e2.getCurrentToken
						if( x == null ) "null" else x.toString
						})
				}
			}
			c2.toList.mkString(",") should equal( currents )
		}
		it("Interrupted parsing - current token test") {
			val c2 = scala.collection.mutable.Queue.empty[String]
			val e2 = JsonEmitter(js)
			var i = 0
			var done = false
			while( !done ) {
				if( i == 12 ) e2.restoreState
				if( i > 5 && i < 12 ) {
					i = i+1
					e2.peekNextToken
				} else {
					val t = e2.nextToken
					if( t == null )
						done = true
					else c2.enqueue( {
						val x = e2.getCurrentToken
						if( x == null ) "null" else x.toString
						})
				}
			}
			c2.toList.mkString(",") should equal( currents )
		}
		it("Must support currentName") {
			val c1 = scala.collection.mutable.Queue.empty[String]
			val e = JsonEmitter(js)
			var done = false
			while( !done ) {
				val t = e.nextToken
				if( t == null )
					done = true
				else c1.enqueue( {
					val x = e.getCurrentName
					if( x == null ) "null" else x.toString
					})
			}
			val currentNames = c1.toList.mkString(",")

			val c2 = scala.collection.mutable.Queue.empty[String]
			val e2 = JsonEmitter(js)
			var i = 0
			done = false
			while( !done ) {
				if( i > 5 && i < 12 ) {
					i = i+1
					e2.peekNextToken
				} else {
					val t = e2.nextToken
					if( t == null )
						done = true
					else c2.enqueue( {
						val x = e2.getCurrentName
						if( x == null ) "null" else x.toString
						})
				}
			}
			c2.toList.mkString(",") should equal( currentNames )
		}
		it("Must support skipChildren") {
			val c1 = scala.collection.mutable.Queue.empty[String]
			val e = (new JsonFactory()).createParser(js)	
			var done = false
			while( !done ) {
				if( e.getCurrentName == "hobbies" && e.getCurrentToken == JsonToken.START_ARRAY ) {
					e.skipChildren
				} else {
					val t  = e.nextToken
					if( t == null ) 
						done = true
					else c1.enqueue({
						val x = e.getCurrentToken
						if( x == null ) "null" else x.toString
						})
				}
			}
			val propperSkip = c1.toList.mkString(",")

			val c2 = scala.collection.mutable.Queue.empty[String]
			val e2 = JsonEmitter(js)
			var i = 0
			done = false
			while( !done ) {
				if( i > 3 && i < 9 ) { // be sure we're queing something in the skipped range!
					i = i+1
					e2.peekNextToken
				} else {
					if( e2.getCurrentName == "hobbies" && e2.getCurrentToken == JsonToken.START_ARRAY ) {
						e2.skipChildren()
					} else {
						val t  = e2.nextToken
						if( t == null ) 
							done = true
						else c2.enqueue({
							val x = e2.getCurrentToken
							if( x == null ) "null" else x.toString
							})
					}
				}
			}
			c2.toList.mkString(",") should equal( propperSkip )
		}
	}
}