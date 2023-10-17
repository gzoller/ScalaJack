package co.blocke.scalajack
package yaml
package misc

import TestUtil._
import munit._
import munit.internal.console

object MyTypes {
  type Phone = String
}
import MyTypes._
import model._

import scala.collection.mutable
import co.blocke.scala_reflection.RType
import co.blocke.scala_reflection.info.AliasInfo

//-----------
opaque type Phone >: Null = String

case class PersonWithPhone(name: String, phone: Phone)

// Override just Phone
object PhoneAdapter extends TypeAdapterFactory with TypeAdapter[Phone]:
  def matches(concrete: RType): Boolean = 
    concrete match {
      case a: AliasInfo if a.name == "Phone" => true
      case _ => false
    }
  def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[Phone] = this
  val info = RType.of[Phone]
  override def isStringish: Boolean = true
  
  def read(parser: Parser): Phone =
    parser.expectString() match {
      case null      => null.asInstanceOf[Phone]
      case s: String => s.replaceAll("-", "").asInstanceOf[Phone]
    }

  def write[WIRE](
      t:      Phone,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit = t match {
    case null => writer.writeNull(out)
    case _ =>
      writer.writeString(
        "%s-%s-%s".format(t.toString.substring(0, 3), t.toString.substring(3, 6), t.toString.substring(6)),
        out
      )
  }

class YamlFlavorSpec extends FunSuite:

  val sj = ScalaJack(YamlFlavor())

  test("parse") {
    describe(
      "-----------------------------\n:  YamlFlavor Tests (YAML)  :\n-----------------------------", Console.BLUE
    )
    val parser = sj.parse("Foo".asInstanceOf[YAML])
    assertEquals(parser.expectString(), "Foo")
  }

  test("allowPermissivePrimitives") {
   interceptMessage[ScalaJackError]("Not available for YAML encoding"){
      (sj.allowPermissivePrimitives())
    }
  }

  test("withAdapters: Overrides type adapter for specific (given) type") {
    val sj2  = sj.withAdapters(PhoneAdapter)
    val inst = PersonWithPhone("Bartholomew", "5555555555".asInstanceOf[Phone])
    val yaml = sj2.render(inst)
    assertEquals("""name: Bartholomew
                    |phone: 555-555-5555
                    |""".stripMargin.asInstanceOf[YAML], yaml )
    assert(inst == sj2.read[PersonWithPhone](yaml))
  }

  test("withDefaultHint") {
    val sj2 = sj.withDefaultHint("foom")
    val yaml =
      """foom: co.blocke.scalajack.yaml.misc.Person
        |name: Fred
        |age: 34
        |""".stripMargin.asInstanceOf[YAML]
    assertEquals(sj2.render[Human](Person("Fred", 34)), yaml)
  }