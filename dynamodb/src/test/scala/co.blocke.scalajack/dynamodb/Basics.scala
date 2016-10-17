package co.blocke.scalajack
package dynamodb
package test

import scala.reflect.runtime.universe.typeOf
import org.scalatest.{ FunSpec, GivenWhenThen, BeforeAndAfterAll }
import org.scalatest.Matchers._
import scala.util.Try
import java.util.UUID

import com.amazonaws.services.dynamodbv2.document.Item

trait Human { val name: String; val age: Int }
case class Misc(wow: Double, bing: String)
case class Person(name: String, age: Int, likes: List[String], stuff: Misc, foo: Option[Boolean] = None) extends Human
trait Address { val postalCode: String }
case class DefaultAddress(postalCode: String) extends Address

class Basics extends FunSpec with GivenWhenThen with BeforeAndAfterAll {

  val sj = ScalaJack(DynamoFlavor())

  describe("-----------------------\n:  Basic Value Tests  :\n-----------------------") {
    describe("Standard Serialization:") {
      it("Basic class with embedded class") {
        val inst: Person = Person("Greg", 50, List("Woodworking", "Diet Coke"), Misc(1.23, "boom"), Some(true))
        val item = sj.render(inst)
        assertResult("""{ Item: {name=Greg, age=50, likes=[Woodworking, Diet Coke], stuff={wow=1.23, bing=boom}, foo=true} }""") { item.toString }
        assertResult(inst) {
          sj.read[Person](item)
        }
      }
      it("Optional default = None") {
        val inst: Person = Person("Greg", 50, List("Woodworking", "Diet Coke"), Misc(1.23, "boom"))
        val item = sj.render(inst)
        assertResult("""{ Item: {name=Greg, age=50, likes=[Woodworking, Diet Coke], stuff={wow=1.23, bing=boom}} }""") { item.toString }
        assertResult(inst) {
          sj.read[Person](item)
        }
      }
      it("Trait support") {
        val inst: Human = Person("Greg", 50, List("Woodworking", "Diet Coke"), Misc(1.23, "boom"), Some(false))
        val item = sj.render(inst)
        assertResult("""{ Item: {_hint=co.blocke.scalajack.dynamodb.test.Person, name=Greg, age=50, likes=[Woodworking, Diet Coke], stuff={wow=1.23, bing=boom}, foo=false} }""") { item.toString }
        assertResult(inst) {
          sj.read[Human](item)
        }
      }
    }

    describe("Extended Serialization:") {
      it("Default Hint") {
        val sj = ScalaJack(DynamoFlavor()).withDefaultHint("kind")
        val inst: Human = Person("Greg", 50, List("Woodworking", "Diet Coke"), Misc(1.23, "boom"), Some(false))
        val item = sj.render(inst)
        assertResult("""{ Item: {kind=co.blocke.scalajack.dynamodb.test.Person, name=Greg, age=50, likes=[Woodworking, Diet Coke], stuff={wow=1.23, bing=boom}, foo=false} }""") { item.toString }
        assertResult(inst) {
          sj.read[Human](item)
        }
      }
      it("ParseOrElse") {
        val sj = ScalaJack(DynamoFlavor()).parseOrElse((typeOf[Address] -> typeOf[DefaultAddress]))
        val item = Item.fromJSON("""{"_hint":"co.blocke.scalajack.test.custom.UnknownAddress","street":"123 Main","city":"New York","state":"NY","postalCode":"39822"}""")
        assertResult(DefaultAddress("39822")) {
          sj.read[Address](item)
        }
      }
      it("No isCanonical") {
        the[java.lang.UnsupportedOperationException] thrownBy
          ScalaJack(DynamoFlavor()).isCanonical(false) should have message "Not available for Dynamo formatting"
      }
    }
  }
}

// def withAdapters(ta: TypeAdapterFactory*) = this.copy(customAdapters = this.customAdapters ++ ta.toList)
// def withHints(h: (Type, String)*) = this.copy(hintMap = this.hintMap ++ h)
// def withHintModifiers(hm: (Type, HintModifier)*) = this.copy(hintModifiers = this.hintModifiers ++ hm)
