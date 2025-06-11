package co.blocke.scalajack
package xml
package classes

//@xmlLabel("bouncer")
case class Ball(size: Int)
case class Car(make: String)

@xmlLabel("dude")
case class Person(
    name: String,
    @xmlLabel("duration") age: Int,
//    @xmlEntryLabel("item") normal: List[Ball],
//    naked: List[Ball],
//    @xmlStruct asStruct: List[Ball],
//    @xmlStruct pet: Animal,
//    oneBall: Ball,
    @xmlStruct @xmlLabel("zoom") road: List[Car],
    @xmlStruct @xmlLabel("boing") ball: Ball
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

sealed trait Animal:
  val legs: Int

case class Dog(legs: Int, food: String) extends Animal

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
)

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
