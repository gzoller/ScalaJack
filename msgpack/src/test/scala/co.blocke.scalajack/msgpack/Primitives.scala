package co.blocke.scalajack
package msgpack
package test

import scala.reflect.runtime.universe.{ typeOf, Type }
import org.scalatest.{ FunSpec, GivenWhenThen, BeforeAndAfterAll }
import org.scalatest.Matchers._
import scala.util.Try
import java.util.UUID
import org.msgpack.MessagePack
import co.blocke.scalajack.msgpack.test.primitives._

trait Bar { val size: Int }
case class Foo(size: Int) extends Bar
case class Person(name: String, age: List[Bar], other: Boolean)
case class PersonS(name: String, age: Int)

case class BMark_Person(id: Long, first_name: String, last_name: String, email: String, gender: String, ip_address: String)
trait Human
case class Male() extends Human
case class Female() extends Human

class Primitives extends FunSpec with GivenWhenThen with BeforeAndAfterAll {

  val sj = ScalaJack(MsgPackFlavor())
  val humanHintMod = new HintModifier {
    def apply(rawHint: String) = rawHint match {
      case "Male"   ⇒ typeOf[Male]
      case "Female" ⇒ typeOf[Female]
    }
    def unapply(hintFieldType: Type) = hintFieldType match {
      case t if (t == typeOf[Male])   ⇒ "Male"
      case t if (t == typeOf[Female]) ⇒ "Female"
    }
  }

  describe("---------------------------\n:  Primitive Value Tests  :\n---------------------------") {
    describe("Standard Serialization:") {

      it("Long must work (not nullable)") {
        val inst = SampleLong(Long.MaxValue, Long.MinValue, 0L, 123L)
        val bytes = sj.render(inst)
        // println(bytes.map("%02X" format _).mkString(" "))
        // assertResult("""{"l1":9223372036854775807,"l2":-9223372036854775808,"l3":0,"l4":123}""") { bytes }
        assertResult(inst) {
          sj.read[SampleLong](bytes)
        }
      }

      /*
      it("Basic class with primitive fields") {
        val inst: Person = Person("Greg", List(Foo(1), Foo(2)), false)
        val bytes = sj.render(inst)
        println(bytes.map("%02X" format _).mkString(" "))

        val again = sj.read[Person](bytes)
        println(again)
      }
      it("Reads") {
        val inst = PersonS("Greg", 50)
        val bytes = sj.render(inst)
        println(bytes.map("%02X" format _).mkString(" "))

        val again = sj.read[PersonS](bytes)
        println(again)
      }
      it("Benchmark") {
        val jsonString = """[{"id":1,"first_name":"Kenneth","last_name":"Watson","email":"kwatson0@goo.ne.jp","gender":"Male","ip_address":"50.27.55.219"},
                     {"id":2,"first_name":"Jason","last_name":"Peters","email":"jpeters1@tinypic.com","gender":"Male","ip_address":"152.156.120.235"},
                     {"id":3,"first_name":"Beverly","last_name":"Stevens","email":"bstevens2@ustream.tv","gender":"Female","ip_address":"169.212.150.35"},
                     {"id":4,"first_name":"Theresa","last_name":"Dixon","email":"tdixon3@hp.com","gender":"Female","ip_address":"137.214.192.32"},
                     {"id":5,"first_name":"Michael","last_name":"Carr","email":"mcarr4@discovery.com","gender":"Male","ip_address":"244.152.168.54"},
                     {"id":6,"first_name":"Carolyn","last_name":"Cruz","email":"ccruz5@nps.gov","gender":"Female","ip_address":"228.112.58.94"},
                     {"id":7,"first_name":"Louis","last_name":"Alexander","email":"lalexander6@mapy.cz","gender":"Male","ip_address":"118.195.8.173"},
                     {"id":8,"first_name":"Laura","last_name":"Campbell","email":"lcampbell7@google.ca","gender":"Female","ip_address":"125.91.1.1"},
                     {"id":9,"first_name":"Judy","last_name":"Burke","email":"jburke8@furl.net","gender":"Female","ip_address":"153.45.26.242"},
                     {"id":10,"first_name":"Earl","last_name":"Stevens","email":"estevens9@discovery.com","gender":"Male","ip_address":"172.161.173.238"},
                     {"id":11,"first_name":"Rose","last_name":"Cooper","email":"rcoopera@lulu.com","gender":"Female","ip_address":"99.128.103.204"},
                     {"id":12,"first_name":"Ashley","last_name":"Hawkins","email":"ahawkinsb@artisteer.com","gender":"Female","ip_address":"128.225.193.155"},
                     {"id":13,"first_name":"Howard","last_name":"Harvey","email":"hharveyc@naver.com","gender":"Male","ip_address":"64.177.55.210"},
                     {"id":14,"first_name":"Edward","last_name":"Ramos","email":"eramosd@is.gd","gender":"Male","ip_address":"208.65.154.100"},
                     {"id":15,"first_name":"Jonathan","last_name":"Gonzalez","email":"jgonzaleze@walmart.com","gender":"Male","ip_address":"166.223.153.41"},
                     {"id":16,"first_name":"Chris","last_name":"Reynolds","email":"creynoldsf@mail.ru","gender":"Male","ip_address":"183.239.230.178"}]"""
        val sjjs = ScalaJack().withHints((typeOf[Human] -> "gender")).withHintModifiers((typeOf[Human] -> humanHintMod))
        val sjmp = ScalaJack(MsgPackFlavor()).withHints((typeOf[Human] -> "gender")).withHintModifiers((typeOf[Human] -> humanHintMod))

        val bytes = sjmp.render(sjjs.read[List[BMark_Person]](jsonString))
        println("Size: " + bytes.length)
        println(sjmp.read[List[BMark_Person]](bytes))
      }
    */
    }
  }
}
