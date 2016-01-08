package co.blocke.scalajack
package test

import mongo._
import org.scalatest.{ FunSpec, GivenWhenThen, BeforeAndAfterAll }
import org.scalatest.Matchers._
import scala.language.postfixOps
import scala.util.Try
import org.joda.time.{DateTime,DateTimeZone}
import org.joda.time.format.DateTimeFormat
import com.mongodb.casbah.Imports._

case class TT2    ( name:String, rec:Map[String,List[(String,Int,Boolean)]])

class TupleSpec extends FunSpec {
	val sjM = ScalaJack(MongoType())

	object MongoMaster {
		val a = MongoDBObject("name"->"Larry","rec"->MongoDBObject("foo"->MongoDBList(MongoDBList("a",1,true)),"hey"->MongoDBList(MongoDBList("x",8,false),MongoDBList("r",3,true))))
	}

	object ScalaMaster {
		val r = List(("a",1,true))
		val r2 = List(("x",8,false),("r",3,true))
		val a = TT2("Larry",Map( "foo"->r,"hey"->r2 ))
	}

	describe("===================\n| -- Any Tests -- |\n===================") {
		it("Render Tests") {
			sjM.render( ScalaMaster.a ) should be( MongoMaster.a )
		}
		it("Read Tests") {
			sjM.read[TT2]( MongoMaster.a ) should be( ScalaMaster.a )
		}
	}
}
