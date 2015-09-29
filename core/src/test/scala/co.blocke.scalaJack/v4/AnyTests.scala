package co.blocke.scalajack
package test.v4

import org.scalatest.{ FunSpec, GivenWhenThen, BeforeAndAfterAll }
import org.scalatest.Matchers._
import scala.language.postfixOps
import scala.util.Try
import org.joda.time.{DateTime,DateTimeZone}
import org.joda.time.format.DateTimeFormat

case class Something(
	name:String,
	stuff:Map[String,Any]
	)

// Complex real-world problem involving type def (Data below) that was breaking
package object v4 {
  type Data = Map[String,Any]
  type QConfig = Map[String,Int]
}
import v4._

object MediaID extends Enumeration {
  type MediaID = Value
  val Email, SMS, Push, Facebook = Value
}
import MediaID._

trait ContactInfo

case class EmailContactInfo() extends ContactInfo
case class SMSContactInfo() extends ContactInfo
case class PushContactInfo() extends ContactInfo
case class FacebookContactInfo() extends ContactInfo

object MsgPriority extends Enumeration {
  type MsgPriority = Value
  val Critical, High, Low = Value
  // Critical messages override opt-out while High does not.  
}
import MsgPriority._
case class Recipient(
  firstName         : String,
  lastName          : String,
  salutation        : String,
  preferredMedia    : MediaID,
  preferredLanguage : String,
  optOut            : List[MediaID],
  contacts          : Map[MediaID,ContactInfo]
  )
trait Command
trait MessageCommand extends Command {
  val intent    : String
  val recipient : Recipient // WARNING-may not have recipient--just an ID here!  Fix this.
}
case class DirectCommand(
  intent    : String,
  priority  : MsgPriority,
  recipient : Recipient,
  data      : Data
) extends MessageCommand

trait AppConfig
case class HelloConfig(
  qIn    : QConfig,
  mediaQ : Map[String,QConfig]
  ) extends AppConfig

case class Harness(
  applications : List[AppConfig]
)
//---------------------------------


class AnySpec extends FunSpec {
	val sjJS  = ScalaJack()

	object JSMaster {
		val a = """{"name":"Fred","stuff":{"a":1,"b":true}}"""
		val b = """{"name":"Fred","stuff":{"a":1,"b":[4,5,6]}}"""
		val c = """{"name":"Fred","stuff":{"a":1,"b":[{"x":"Fido","y":false},{"x":"Cat","y":true}]}}"""
		val d = """{"name":"Fred","stuff":{"a":1,"b":null}}"""
		val e = """{"name":"Fred","stuff":{"a":1,"b":["foo",null,"bar"]}}"""
		val f = """{"name":"Fred","age":40}"""
		val g = """{"name":"Fred","stuff":{"a":1,"b":"Greg"}}"""
		val x = """{"_hint":"co.blocke.scalajack.test.v4.DirectCommand","intent":"hello","priority":"High","recipient":{"firstName":"John","lastName":"Smith","salutation":"Mr.","preferredMedia":"Email","preferredLanguage":"en_US","optOut":[],"contacts":{}},"data":{"a":1,"b":45}}"""
		val y = """{"applications":[{"_hint":"co.blocke.scalajack.test.v4.HelloConfig","qIn":{"Critical":2},"mediaQ":{"Email":{"Foo":15}}}]}"""
	}

	object ScalaMaster {
		val a = Something("Fred",Map("a"->1,"b"->true))
		val b = Something("Fred",Map("a"->1,"b"->List(4,5,6)))
		val c = Something("Fred",Map("a"->1,"b"->List(Map("x"->"Fido","y"->false),Map("x"->"Cat","y"->true))))
		val d = Something("Fred",Map("a"->1,"b"->null))
		val e = Something("Fred",Map("a"->1,"b"->List("foo",null,"bar")))
		val f = Map[String,Any]("name"->"Fred","age"->40)
		val g = Something("Fred",Map("a"->1,"b"->"Greg"))
		val x = DirectCommand(
			"hello",
			MsgPriority.High,
			Recipient("John","Smith","Mr.",MediaID.Email,"en_US",List.empty[MediaID],Map.empty[MediaID,ContactInfo]),
			Map("a"->1,"b"->45)
			)
		val y = Harness(List(HelloConfig(Map("Critical" -> 2),Map("Email" -> Map("Foo" -> 15)))))
	}

	describe("===================\n| -- Any Tests -- |\n===================") {
		describe("Render Tests") {
			sjJS.render( ScalaMaster.a ) should be( JSMaster.a )
			sjJS.render( ScalaMaster.b ) should be( JSMaster.b )
			sjJS.render( ScalaMaster.c ) should be( JSMaster.c )
			sjJS.render( ScalaMaster.d ) should be( JSMaster.d )
			sjJS.render( ScalaMaster.e ) should be( JSMaster.e )
			sjJS.render( ScalaMaster.f ) should be( JSMaster.f )
			sjJS.render( ScalaMaster.g ) should be( JSMaster.g )
			sjJS.render[Command]( ScalaMaster.x ) should be( JSMaster.x )
			sjJS.render( ScalaMaster.y ) should be( JSMaster.y )
		}
		describe("Read Tests") {
			sjJS.read[Something](JSMaster.a).stuff should contain allOf (("a" -> 1), ("b" -> true))
			sjJS.read[Something](JSMaster.b).stuff should contain allOf (("a" -> 1), ("b" -> List(4,5,6)))
			val c = sjJS.read[Something](JSMaster.c).stuff.asInstanceOf[Map[String,List[Map[_,_]]]] 
			c("b")(0) should contain allOf (("x" -> "Fido"), ("y" -> false))
			c("b")(1) should contain allOf (("x" -> "Cat"), ("y" -> true))
			sjJS.read[Something](JSMaster.d).stuff should contain allOf (("a" -> 1), ("b" -> null))
			sjJS.read[Something](JSMaster.e).stuff should contain allOf (("a" -> 1), ("b" -> List("foo",null,"bar")))
			sjJS.read[Map[String,Any]](JSMaster.f) should contain allOf (("name" -> "Fred"), ("age" -> 40))
			sjJS.read[Something](JSMaster.g).stuff should contain allOf (("a" -> 1), ("b" -> "Greg"))
			sjJS.read[Command](JSMaster.x) should be( ScalaMaster.x )
			sjJS.read[Harness](JSMaster.y) should be( ScalaMaster.y )
		}
	}
}
