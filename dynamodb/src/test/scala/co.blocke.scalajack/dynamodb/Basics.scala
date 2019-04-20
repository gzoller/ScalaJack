package co.blocke.scalajack
package dynamodb

import model._

import scala.reflect.runtime.universe.typeOf
import org.scalatest.{ FunSpec, GivenWhenThen, BeforeAndAfterAll }

import com.amazonaws.services.dynamodbv2.document.Item

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
        assertResult("""{ Item: {_hint=co.blocke.scalajack.dynamodb.Person, name=Greg, age=50, likes=[Woodworking, Diet Coke], stuff={wow=1.23, bing=boom}, foo=false} }""") { item.toString }
        assertResult(inst) {
          sj.read[Human](item)
        }
      }
    }

    describe("Extended Serialization:") {
      it("Custom type adapter") {
        val sj = ScalaJack(DynamoFlavor()).withAdapters(PhoneAdapter)
        val inst = PersonWithPhone("Bartholomew", "5555555555")
        val item = sj.render(inst)
        assertResult("""{ Item: {name=Bartholomew, phone=555-555-5555} }""") { item.toString }
        assertResult(inst) {
          sj.read[PersonWithPhone](item)
        }
      }
      it("With Hints") {
        val sj = ScalaJack(DynamoFlavor()).withHints((typeOf[Address] -> "addr_kind"))
        val inst: Address = USAddress("123 Main", "New York", "NY", "39822")
        val item = sj.render(inst)
        assertResult("""{ Item: {addr_kind=co.blocke.scalajack.dynamodb.USAddress, street=123 Main, city=New York, state=NY, postalCode=39822} }""") { item.toString }
        assertResult(inst) {
          sj.read[Address](item)
        }
      }
      it("With Hint Modifiers") {
        val prependHintMod = ClassNameHintModifier((hint: String) => "co.blocke.scalajack.dynamodb." + hint, (cname: String) => cname.split('.').last)
        val sj = ScalaJack(DynamoFlavor()).withHintModifiers((typeOf[Address], prependHintMod))
        val inst: Address = USAddress("123 Main", "New York", "NY", "39822")
        val item = sj.render(inst)
        assertResult("""{ Item: {_hint=USAddress, street=123 Main, city=New York, state=NY, postalCode=39822} }""") { item.toString }
        assertResult(inst) {
          sj.read[Address](item)
        }
      }
      it("Externalized type modifier") {
        val sj = ScalaJack(DynamoFlavor()).withTypeValueModifier(ClassNameHintModifier((hint: String) => "co.blocke.scalajack.dynamodb." + hint, (cname: String) => cname.split('.').last))
        val value = Envelope("DEF", FancyBody("BOO"))
        val item = sj.render(value)
        assertResult("{ Item: {Giraffe=FancyBody, id=DEF, body={message=BOO}} }") { item.toString }
        assertResult(value) {
          sj.read[Envelope[Body]](item)
        }
      }
      it("Default Hint") {
        val sj = ScalaJack(DynamoFlavor()).withDefaultHint("kind")
        val inst: Human = Person("Greg", 50, List("Woodworking", "Diet Coke"), Misc(1.23, "boom"), Some(false))
        val item = sj.render(inst)
        assertResult("""{ Item: {kind=co.blocke.scalajack.dynamodb.Person, name=Greg, age=50, likes=[Woodworking, Diet Coke], stuff={wow=1.23, bing=boom}, foo=false} }""") { item.toString }
        assertResult(inst) {
          sj.read[Human](item)
        }
      }
      it("ParseOrElse") {
        val sj = ScalaJack(DynamoFlavor()).parseOrElse((typeOf[Address] -> typeOf[DefaultAddress]))
        val item = Item.fromJSON("""{"_hint":"co.blocke.scalajack.custom.UnknownAddress","street":"123 Main","city":"New York","state":"NY","postalCode":"39822"}""")
        assertResult(DefaultAddress("39822")) {
          sj.read[Address](item)
        }
      }
    }
  }
}