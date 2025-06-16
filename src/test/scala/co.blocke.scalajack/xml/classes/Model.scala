package co.blocke.scalajack
package xml
package classes

import co.blocke.scala_reflection.Ignore

case class Person(name: String, @xmlLabel("duration") age: Int)

class Parent(val phase: Int, @xmlEntryLabel("one") var stuff: List[String]):
  private var _hidden: Boolean = false

  def hidden: Boolean = _hidden
  def hidden_=(h: Boolean) = _hidden = h

  private var _nope: Boolean = false // should not generate due to @Ignore
  @Ignore def nope: Boolean = _nope
  def nope_=(h: Boolean) = _nope = h

  var foo: String = "ok"
  @Ignore var noFoo: String = "not ok"

case class Child(name: String, age: Int, override val phase: Int) extends Parent(phase, Nil)

case class Params[X, Y](a: List[X], b: Option[Y])

sealed trait Command
case object Start extends Command
case object Stop extends Command

sealed trait Animal
@TypeHint(hintValue = "bowow")
case class Dog(name: String, numLegs: Int) extends Animal
@TypeHint(hintValue = "flipper")
case class Fish(species: String, freshwater: Boolean) extends Animal

sealed trait City
class Dallas(val pop: Int) extends City
@TypeHint(hintValue = "vice")
class Miami(val temp: Double) extends City

sealed trait Route
class CityRoute(val numStreets: Int) extends Route
// Testing indirection. In real-world scenario all your sealed trait's classes
// must be defined in one file. Implementation classes like CityRouteImpl could
// be in other files so the sealed trait's file doesn't grow huge.

case class TraitHolder(a: Command, b: Animal, c: City, d: Route)

sealed abstract class Command2
case object Start2 extends Command2
case object Stop2 extends Command2

sealed abstract class Animal2
@TypeHint(hintValue = "bowow")
case class Dog2(name: String, numLegs: Int) extends Animal2
@TypeHint(hintValue = "flipper")
case class Fish2(species: String, freshwater: Boolean) extends Animal2

sealed abstract class City2
class Dallas2(val pop: Int) extends City2
@TypeHint(hintValue = "vice")
class Miami2(val temp: Double) extends City2

sealed abstract class AThing[T]
class Thing1[T](val t: T) extends AThing[T]
class Thing2[T](val t: T, val s: String) extends AThing[T]

case class AbstractClassHolder(a: Command2, b: Animal2, c: City2)

case class AbstractClassHolder2[P](a: AThing[P])

case class Empl[T](id: String, data: T, boss: Empl[T], @xmlEntryLabel("one") coworkers: List[Empl[T]])

// Value class
class Email(val value: String) extends AnyVal {
  def domain: String = value.split("@").last
}
case class ValueClassHolder(id: Int, email: Email)

object VehicleClass extends Enumeration {
  type VehicleClass = Value
  val Land, Air, Sea = Value
}

import VehicleClass.*

sealed trait Vehicle {
  val kind: VehicleClass
}

case class Car(passengers: Int) extends Vehicle {
  val kind: Land.type = Land
}

sealed trait Hobby[X, Y] {
  val thing1: X;
  val thing2: Y
}

sealed trait Artist[W, Z] {
  val instrument: W;
  val effort: Z
}

sealed trait PersonX[X, Y] {
  val who: X;
  val org: Y
}

case class Sports[A, B](thing1: A, thing2: B) extends Hobby[A, B]

case class Painter[A, B](instrument: A, effort: B) extends Artist[A, B]

case class Employee[A, B, C, D](who: Artist[C, Hobby[D, A]], org: B) extends PersonX[Artist[C, Hobby[D, A]], B]

type ComplexPerson = PersonX[Artist[Int, Hobby[Double, Char]], Vehicle]

sealed trait Machine
case class Press(name: String, lbs: Int) extends Machine
case class Lift(name: String, lbs: Int, foo: Boolean) extends Machine
case class Drill(name: String, numBits: Int) extends Machine
case class Swing(name: String, lbs: Option[Int], isBig: Boolean) extends Machine
case class MachineHolder(m1: Machine, m2: Machine, m3: Machine, m4: Machine)

// These should always generate a type hint!
sealed trait Machine2
case class Press2(name: String, lbs: Option[Int]) extends Machine2
case class Lift2(name: String, lbs: Option[Int]) extends Machine2
case class Drill2(name: String, lbs: Option[Int], numBits: Int) extends Machine2
case class MachineHolder2(m1: Machine2, m2: Machine2, m3: Machine2)

// Unique field key of "" and complex (nested) trait
sealed trait Level0
case class L0A(x: Int, name: Option[String], y: Boolean) extends Level0
case class L0B(name: Option[String], id: String, blather: Option[String]) extends Level0
case class L0C(id: Option[Int], extra: List[Int]) extends Level0

sealed trait Level1
case class L1R(blather: String, name: Option[String], l0: Level0) extends Level1
case class L1S(id: Long, nombre: String) extends Level1
case class L1Q(name: String, age: Int, l0: Level0) extends Level1
case class L1X(name: String, age: Int, l0: Level0) extends Level1
case class ComplexHolder(c1: Level1, c2: Level1, c3: Level1, c4: Level1)

// Self-reference for Traits
sealed trait Outer
case class OuterImpl(name: String, num: Int, stuff: List[Outer]) extends Outer

//----------------------------------------------------------------------------------------
//@xmlLabel("bouncer")
case class BallG(size: Int)
case class CarG(make: String)

@xmlLabel("dude")
case class PersonG(
    name: String,
    @xmlLabel("duration") age: Int,
//    @xmlEntryLabel("item") normal: List[Ball],
//    naked: List[Ball],
//    @xmlStruct asStruct: List[Ball],
//    @xmlStruct pet: Animal,
//    oneBall: Ball,
    @xmlStruct @xmlLabel("zoom") road: List[CarG],
    @xmlStruct @xmlLabel("boing") ball: BallG
//    dunno: Option[Ball],
//    @xmlStruct kinda: Option[Ball]
//    dunno2: Option[List[Ball]],
//    @xmlLabel("thingy") @xmlStruct kinda2: Option[List[Ball]]
) //, @xmlEntryLabel("bip")items: Map[String,Int])

/*
So at this point we have:
 * Simple case classes
 * Primmitive data types
 * Lists
 * Label and entry renaming
 * Maps -- in-progress

Next:
 * Option (empty elements)
 */

sealed trait AnimalG:
  val legs: Int

case class DogG(legs: Int, food: String) extends AnimalG

@xmlLabel("_DOC")
case class Invoice810(
    @xmlLabel("TYPE") `type`: String,
    recid: String,
    invoiceno: String,
    invoicedate: String,
    orderno: String,
    ordertype: String,
    orderdate: String,
    purchaseorderno: String,
    cusno: String,
    division: String,
    department: String,
    shipdate: String,
    canceldate: String,
    collect: String,
    bolno: String,
    iscreditmemo: String,
    miscamount: Double,
    freightamount: Double,
    totinvoiceamount: Double,
    tottaxableamount: Double,
    totsalesamount: Double,
    totcartons: Int,
    totweight: Double,
    lineitemtotal: Int,
    @xmlStruct @xmlLabel("_CARRIER") carrier: Carrier,
    @xmlStruct @xmlLabel("_MESSAGE") message: List[Message],
    @xmlStruct @xmlLabel("_TERMS") terms: Terms,
    @xmlStruct @xmlLabel("_CURRENCY") currency: Currency,
    @xmlStruct @xmlLabel("_CREDITMEMO") creditmemo: CreditMemo,
    @xmlStruct @xmlLabel("_ADDRESS") address: List[Address],
    @xmlStruct @xmlLabel("_TAX") tax: Tax,
    @xmlStruct @xmlLabel("_USERDEF") userdef: List[Userdef],
    @xmlStruct @xmlLabel("_ITEM") item: List[Item]
):
  def toX12: String =
    def segment(elems: String*): String = elems.mkString("", "*", "~\n")

    val header = new StringBuilder

    header ++= segment("ISA", "00", "", "00", "", "ZZ", "SENDERID", "ZZ", "RECEIVERID", "240603", "1200", "U", "00401", "000000001", "0", "P", ">")
    header ++= segment("GS", "IN", "SENDERID", "RECEIVERID", "20240603", "1200", "1", "X", "005010")
    header ++= segment("ST", "810", recid)
    header ++= segment("BIG", invoicedate, invoiceno, orderdate, orderno)

    // Loop N1 - Bill To
    address.find(_.`type` == "billto").foreach { addr =>
      header ++= segment("N1", "BT", addr.name, "92", addr.id)
      if addr.add1.nonEmpty then header ++= segment("N3", addr.add1)
      if addr.city.nonEmpty || addr.state.nonEmpty || addr.zip.nonEmpty then header ++= segment("N4", addr.city, addr.state, addr.zip)
    }

    // Loop N1 - Ship To
    address.find(_.`type` == "shipto").foreach { addr =>
      header ++= segment("N1", "ST", addr.name, "92", addr.id)
      if addr.add1.nonEmpty then header ++= segment("N3", addr.add1)
      if addr.city.nonEmpty || addr.state.nonEmpty || addr.zip.nonEmpty then header ++= segment("N4", addr.city, addr.state, addr.zip)
    }

    // ITD - Payment Terms
    val t = terms
    header ++= segment("ITD", "", "", t.discountpercent.toString, "", t.discountdays.toString, "", t.duedays.toString)

    // TDS - Total Monetary Value Summary (amount in cents)
    header ++= segment("TDS", (totinvoiceamount * 100).toInt.toString)

    // Loop IT1 - Line items
    item.zipWithIndex.foreach { case (item, i) =>
      header ++= segment(
        "IT1",
        (i + 1).toString,
        item.qtytoship.toString,
        item.uom,
        item.price.toString,
        "",
        "BP",
        item.itemid,
        "VP",
        item.cusitemid
      )
      if item.itemdesc.nonEmpty then header ++= segment("PID", "F", "", "", "", item.itemdesc)
    }

    // Tax summary
    if tax.taxamt > 0 then header ++= segment("TXI", "TX", tax.taxamt.toString, "", "", tax.taxpercent.toString)

    // CTT - Transaction Totals
    header ++= segment("CTT", item.size.toString)

    // SE - Transaction set trailer
    val segmentCount = header.toString().count(_ == '~') + 1
    header ++= segment("SE", segmentCount.toString, recid)
    header ++= segment("GE", "1", "1")
    header ++= segment("IEA", "1", "000000001")

    header.toString()

case class Carrier(
    carrierid: String,
    carrierdesc: String
)

case class Message(
    @xmlLabel("TYPE") `type`: String,
    message: String
)

case class Terms(
    id: String,
    desc: String,
    duedays: Int,
    discountdays: Int,
    discountpercent: Int,
    discountdate: String,
    datedue: String
)

case class Currency(
    currencycode: String,
    currencyrate: Double
)

case class CreditMemo(
    origordtype: String,
    origordno: String,
    origorddate: String,
    applytono: Int
)

case class Address(
    @xmlLabel("TYPE") `type`: String,
    id: String,
    name: String,
    add1: String,
    add2: String,
    add3: String,
    city: String,
    state: String,
    zip: String,
    country: String,
    contact: String,
    phone: String,
    fax: String,
    email: String,
    @xmlStruct @xmlLabel("_USERDEF") userdef: List[Userdef]
)

case class Userdef(
    @xmlLabel("TYPE") `type`: String,
    userdef: String
)

case class Tax(
    taxsched: String,
    taxcode: String,
    taxableamount: Double,
    taxamt: Double,
    taxpercent: Double,
    taxstate: String
)

case class Item(
    recid: String,
    lineno: Double,
    itemid: String,
    cusitemid: String,
    itemdesc: String,
    itemdesc2: String,
    price: Double,
    extendedprice: Double,
    taxable: String,
    taxflag: String,
    extendedtaxamount: String,
    qtyord: Double,
    qtytoship: Double,
    uom: String,
    requestdate: String,
    promisedate: String,
    requestedshipdate: String,
    pickdate: String,
    shipdate: String,
    qtyreturntostk: String,
    reasoncd: String,
    @xmlStruct @xmlLabel("_MESSAGE") message: List[Message],
    @xmlStruct @xmlLabel("_USERDEF") userdef: List[Userdef],
    @xmlStruct @xmlLabel("_ITEMTAX") itemtax: ItemTax
)

case class ItemTax(
    taxableamt: Double,
    taxamount: Double,
    taxsched: String,
    taxcd: String
)
