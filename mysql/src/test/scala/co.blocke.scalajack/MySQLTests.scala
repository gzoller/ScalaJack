package co.blocke.scalajack

import mysql._
import org.scalatest.{ FunSpec, GivenWhenThen, BeforeAndAfterAll }
import java.sql.Connection
import org.scalatest.Matchers._
import scala.util.Try
import java.util.UUID
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import scala.sys.process._

class MySQLTests extends FunSpec with GivenWhenThen with BeforeAndAfterAll {

	override def beforeAll() {
		// clean database for test
		("mysql --user=root" #< new java.io.File("./mysql/src/test/resources/mysql.sql")).run
		db.addDbs( Map("test"->"jdbc:mysql://localhost:3306/scalajack?user=root&password=&zeroDateTimeBehavior=convertToNull") )
	}

	describe("=====================\n| -- MySQL Tests -- |\n=====================") {
		describe("Select Tests") {
			it("Must retrieve simple records from db") {
				db.withConnection("test"){ implicit c:Connection => {
					val di = ScalaJack.select[Person]("")
					val z = di.toList
					z should equal(List(Person("Mary",24,0.39), Person("Tom",35,0.14)))
					}}
			}
			it("Must retrieve complex records from db") {
				db.withConnection("test"){ implicit c:Connection => {
					val di = ScalaJack.select[Stuff]("")
					val z = di.toList
					z should equal(List(Stuff("wow",5,true,None), Stuff("yikes",7,true,Some("ok"))))
					}}
			}
			it("Must retrieve single record from db") {
				db.withConnection("test"){ implicit c:Connection => {
					val di = ScalaJack.select[Person]("""name="Tom" """)
					val z = di.toList
					z should equal(List(Person("Tom",35,0.14)))
					}}
			}
		}
		describe("Insert Tests") {
			it("Basic multi-entry insert") {
				db.withConnection("test"){ implicit c:Connection => {
					val p1 = Person("Tom",12,0.2)
					val p2 = Person("Bill",78,0.1)
					val di = ScalaJack.insertInto( List(p1,p2) )
					}}
			}
		}
	}
}
