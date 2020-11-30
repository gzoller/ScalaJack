package co.blocke.scalajack
package dynamodb

import model._
import co.blocke.scala_reflection.RType

import TestUtil._
import munit._
import munit.internal.console

import com.amazonaws.services.dynamodbv2.document.Item

class Basics extends FunSuite:

  val sj = ScalaJack(DynamoFlavor())

  test("Basic class with embedded class") {
    describe(
      "--------------------------------\n:  DynamoDB Basic Value Tests  :\n--------------------------------", Console.BLUE
    )
    describe("Standard Serialization:")
    val inst: Person = Person(
      "Greg",
      50,
      List("Woodworking", "Diet Coke"),
      Misc(1.23, "boom"),
      Some(true)
    )
    val item = sj.render(inst)
    assertEquals(
      """{ Item: {name=Greg, age=50, likes=[Woodworking, Diet Coke], stuff={wow=1.23, bing=boom}, foo=true} }""",
      item.toString
    )
    assertEquals(inst, sj.read[Person](item))
  }

  test("Optional default = None") {
    val inst: Person = Person(
      "Greg",
      50,
      List("Woodworking", "Diet Coke"),
      Misc(1.23, "boom")
    )
    val item = sj.render(inst)
    assertEquals(
      """{ Item: {name=Greg, age=50, likes=[Woodworking, Diet Coke], stuff={wow=1.23, bing=boom}} }""",
      item.toString
    )
    assertEquals(inst, sj.read[Person](item))
  }

  test("Trait support") {
    val inst: Human = Person(
      "Greg",
      50,
      List("Woodworking", "Diet Coke"),
      Misc(1.23, "boom"),
      Some(false)
    )
    val item = sj.render(inst)
    assertEquals(
      """{ Item: {_hint=co.blocke.scalajack.dynamodb.Person, name=Greg, age=50, likes=[Woodworking, Diet Coke], stuff={wow=1.23, bing=boom}, foo=false} }""",
      item.toString
    )
    assertEquals(inst, sj.read[Human](item))
  }

  test("Custom type adapter") {
    describe("Extended Serialization:")
    val sj = ScalaJack(DynamoFlavor()).withAdapters(PhoneAdapter)
    val inst = PersonWithPhone("Bartholomew", "5555555555".asInstanceOf[Phone])
    val item = sj.render(inst)
    assertEquals("""{ Item: {name=Bartholomew, phone=555-555-5555} }""", item.toString)
    assertEquals(inst, sj.read[PersonWithPhone](item))
  }

  test("With Hints") {
    val sj =
      ScalaJack(DynamoFlavor()).withHints((RType.of[Address] -> "addr_kind"))
    val inst: Address = USAddress("123 Main", "New York", "NY", "39822")
    val item = sj.render(inst)
    assertEquals(
      """{ Item: {addr_kind=co.blocke.scalajack.dynamodb.USAddress, street=123 Main, city=New York, state=NY, postalCode=39822} }""",
      item.toString
    )
    assertEquals(inst, sj.read[Address](item))
  }

  test("With Hint Modifiers") {
    val prependHintMod = ClassNameHintModifier(
      (hint: String) => "co.blocke.scalajack.dynamodb." + hint,
      (cname: String) => cname.split('.').last
    )
    val sj = ScalaJack(DynamoFlavor())
      .withHintModifiers((RType.of[Address], prependHintMod))
    val inst: Address = USAddress("123 Main", "New York", "NY", "39822")
    val item = sj.render(inst)
    assertEquals(
      """{ Item: {_hint=USAddress, street=123 Main, city=New York, state=NY, postalCode=39822} }""",
      item.toString
    )
    assertEquals(inst, sj.read[Address](item))
  }

  test("Externalized type modifier") {
    val sj = ScalaJack(DynamoFlavor()).withTypeValueModifier(
      ClassNameHintModifier(
        (hint: String) => "co.blocke.scalajack.dynamodb." + hint,
        (cname: String) => cname.split('.').last
      )
    )
    val value = Envelope("DEF", FancyBody("BOO"))
    val item = sj.render(value)
    assertEquals(
      "{ Item: {Giraffe=FancyBody, id=DEF, body={message=BOO}} }",
      item.toString
    )
    assert(value == sj.read[Envelope[Body]](item))
  }

  test("Default Hint") {
    val sj = ScalaJack(DynamoFlavor()).withDefaultHint("kind")
    val inst: Human = Person(
      "Greg",
      50,
      List("Woodworking", "Diet Coke"),
      Misc(1.23, "boom"),
      Some(false)
    )
    val item = sj.render(inst)
    assertEquals(
      """{ Item: {kind=co.blocke.scalajack.dynamodb.Person, name=Greg, age=50, likes=[Woodworking, Diet Coke], stuff={wow=1.23, bing=boom}, foo=false} }""",
      item.toString 
    )
    assertEquals(inst, sj.read[Human](item))
  }

  test("ParseOrElse") {
    val sj = ScalaJack(DynamoFlavor())
      .parseOrElse((RType.of[Address] -> RType.of[DefaultAddress]))
    val item = Item.fromJSON(
      """{"_hint":"co.blocke.scalajack.custom.UnknownAddress","street":"123 Main","city":"New York","state":"NY","postalCode":"39822"}"""
    )
    assert(DefaultAddress("39822") == sj.read[Address](item))
  }
