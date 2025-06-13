package co.blocke.scalajack
package xml
package classes

import ScalaJack.*
import co.blocke.scala_reflection.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.*
import TestUtil.*

class ClassSpec() extends AnyFunSpec:
  opaque type phone = String

  describe(colorString("-------------------------------\n:      XML Class Tests        :\n-------------------------------", Console.YELLOW)) {
    /*
    it("Simple case class must work (with field renaming)") {
      val inst = Person("Bob", 34)
      val sj = sjXmlCodecOf[Person]
      val x = sj.toXml(inst)
      x should equal("""<Person><name>Bob</name><duration>34</duration></Person>""")
      sj.fromXml(x) shouldEqual (inst)
    }
    it("Parameterized class must work") {
      val num: phone = "123-456-7890"
      val inst = Params(List(Person("Bob", 34), Person("Sarah", 28)), Some(num))
      val sj = sjXmlCodecOf[Params[Person, phone]]
      val x = sj.toXml(inst)
      x should equal("""<Params><a><Person><name>Bob</name><duration>34</duration></Person><Person><name>Sarah</name><duration>28</duration></Person></a><b>123-456-7890</b></Params>""")
      sj.fromXml(x) shouldEqual (inst)
    }
     */
    it("Non-constructor fields of class must work") {
      val inst = Parent(99, List("x", "y"))
      inst.hidden_=(true)
      inst.nope_=(false)
      inst.foo = "we'll see"
      val sj = sjXmlCodecOf[Parent](SJConfig.writeNonConstructorFields)
      val x = sj.toXml(inst)
      x should equal("""{"phase":99,"stuff":["x","y"],"foo":"we'll see","hidden":true}""")
      val scrambled = """{"hidden":true,"phase":99,"foo":"we'll see","stuff":["x","y"]}"""
      val re = sj.fromXml(x)
      re.phase shouldEqual (inst.phase)
      re.stuff shouldEqual (inst.stuff)
      re.foo shouldEqual (inst.foo)
      re.hidden shouldEqual (inst.hidden)
    }
    /*
    it("Inherited class must work") {
      val inst = Child("Bob", 34, 3)
      val sj = sjCodecOf[Child]
      val js = sj.toJson(inst)
      js should matchJson("""{"name":"Bob","age":34,"phase":3}""")
      sj.fromJson(js) shouldEqual (inst)
    }
    it("Block non-constructor fields of class must work") {
      val inst = Parent(99, List("x", "y"))
      inst.hidden_=(true)
      inst.nope_=(false)
      val sj = sjCodecOf[Parent]
      val js = sj.toJson(inst)
      js should matchJson("""{"phase":99,"stuff":["x","y"]}""")
      val re = sj.fromJson(js)
      re.phase shouldEqual (inst.phase)
      re.stuff shouldEqual (inst.stuff)
      re.foo shouldEqual ("ok")
      re.hidden shouldEqual (false)
    }
    it("Sealed abstract class with case objects and case classes must work") {
      val inst = AbstractClassHolder(Start2, Fish2("Beta", false), Miami2(101.1))
      val sj = sjCodecOf[AbstractClassHolder]
      val js = sj.toJson(inst)
      js should matchJson("""{"a":"Start2","b":{"species":"Beta","freshwater":false},"c":{"temp":101.1}}""")
      val re = sj.fromJson(js)
      re.a shouldEqual (inst.a)
      re.b shouldEqual (inst.b)
      (re.c.asInstanceOf[Miami2].temp == inst.c.asInstanceOf[Miami2].temp) shouldEqual (true)
    }
    it("Sealed abstract class with modified type hint label must work") {
      val inst = AbstractClassHolder(Start2, Fish2("Beta", false), Miami2(101.1))
      val sj = sjCodecOf[AbstractClassHolder](SJConfig.preferTypeHints.withTypeHintLabel("ref"))
      val js = sj.toJson(inst)
      js should matchJson("""{"a":"Start2","b":{"ref":"Fish2","species":"Beta","freshwater":false},"c":{"ref":"Miami2","temp":101.1}}""")
      val re = sj.fromJson(js)
      re.a shouldEqual (inst.a)
      re.b shouldEqual (inst.b)
      (re.c.asInstanceOf[Miami2].temp == inst.c.asInstanceOf[Miami2].temp) shouldEqual (true)
    }
    it("Sealed abstract class with type hint policy SCRAMBLE_CLASSNAME label must work") {
      val inst = AbstractClassHolder(Start2, Fish2("Beta", false), Miami2(101.1))
      val sj = sjCodecOf[AbstractClassHolder](SJConfig.preferTypeHints.withTypeHintPolicy(TypeHintPolicy.SCRAMBLE_CLASSNAME))
      val js = sj.toJson(inst)
      val diff = parseJValue(js).diff(parseJValue("""{"a":"Start2","b":{"_hint":"82949-049-49A","species":"Beta","freshwater":false},"c":{"_hint":"53150-867-73B","temp":101.1}}"""))
      val diffMap = diff.changed.values.asInstanceOf[Map[String, Map[String, ?]]]
      assert(diffMap("b").contains("_hint") && diffMap("c").contains("_hint")) // ie only the scrambled _hint values are different
      val re = sj.fromJson(js)
      re.a shouldEqual (inst.a)
      re.b shouldEqual (inst.b)
      (re.c.asInstanceOf[Miami2].temp == inst.c.asInstanceOf[Miami2].temp) shouldEqual (true)
    }
    it("Sealed abstract class with type hint policy USE_ANNOTATION label must work") {
      val inst = AbstractClassHolder(Start2, Fish2("Beta", false), Miami2(101.1))
      val sj = sjCodecOf[AbstractClassHolder](SJConfig.preferTypeHints.withTypeHintPolicy(TypeHintPolicy.USE_ANNOTATION))
      val js = sj.toJson(inst)
      js should matchJson("""{"a":"Start2","b":{"_hint":"flipper","species":"Beta","freshwater":false},"c":{"_hint":"vice","temp":101.1}}""")
      val re = sj.fromJson(js)
      re.a shouldEqual (inst.a)
      re.b shouldEqual (inst.b)
      (re.c.asInstanceOf[Miami2].temp == inst.c.asInstanceOf[Miami2].temp) shouldEqual (true)
    }
    it("Parameterized sealed abstract class must work") {
      val inst = AbstractClassHolder2(Thing2(15L, "wow"))
      val sj = sjCodecOf[AbstractClassHolder2[Long]]
      val js = sj.toJson(inst)
      js should matchJson("""{"a":{"t":15,"s":"wow"}}""")
      val re = sj.fromJson(js)
      re.a.asInstanceOf[Thing2[Long]].t shouldEqual (15L)
      re.a.asInstanceOf[Thing2[Long]].s shouldEqual ("wow")
    }
    it("Top-level abstract class must work") {
      val inst: AThing[Long] = Thing2(99L, "ok")
      val sj = sjCodecOf[AThing[Long]]
      val js = sj.toJson(inst)
      js should matchJson("""{"t":99,"s":"ok"}""")
      val re = sj.fromJson(js)
      re.asInstanceOf[Thing2[Long]].t shouldEqual (99L)
      re.asInstanceOf[Thing2[Long]].s shouldEqual ("ok")
    }
    it("Self-referencing class must work (bonus: parameterized self-referencing class)") {
      val inst = Empl("abc123", 5, Empl("xyz11", -1, null, Nil), List(Empl("tru777", 0, null, Nil), Empl("pop9", 9, null, Nil)))
      val sj = sjCodecOf[Empl[Int]]
      val js = sj.toJson(inst)
      js should matchJson("""{"id":"abc123","data":5,"boss":{"id":"xyz11","data":-1,"boss":null,"coworkers":[]},"coworkers":[{"id":"tru777","data":0,"boss":null,"coworkers":[]},{"id":"pop9","data":9,"boss":null,"coworkers":[]}]}""")
      sj.fromJson(js) shouldEqual (inst)
    }
    it("Java classes must work") {
      val inst = new SampleClass()
      inst.setName("John Doe")
      inst.setAge(45)
      inst.setAddress("123 Main St")
      val sj = sjCodecOf[SampleClass]
      val js = sj.toJson(inst)
      js should matchJson("""{"address":"123 Main St","name":"John Doe"}""")
      val re = sj.fromJson(js)
      re.getName shouldEqual ("John Doe")
      re.getAge shouldEqual (0)
      re.getAddress shouldEqual ("123 Main St")
    }
    it("Java class value is null") {
      val inst: SampleClass = null
      val sj = sjCodecOf[SampleClass]
      val js = sj.toJson(inst)
      js shouldEqual ("""null""")
      sj.fromJson(js) shouldEqual (null)
    }
    it("Abstract class value is null") {
      val inst = AbstractClassHolder(null, null, null)
      val sj = sjCodecOf[AbstractClassHolder]
      val js = sj.toJson(inst)
      js should matchJson("""{"a":null,"b":null,"c":null}""")
      val re = sj.fromJson(js)
      re.a shouldEqual (null)
      re.b shouldEqual (null)
      re.c shouldEqual (null)
    }
    it("Scala case class value is null") {
      val inst: Person = null
      val sj = sjCodecOf[Person]
      val js = sj.toJson(inst)
      js shouldEqual ("""null""")
      sj.fromJson(js) shouldEqual (inst)
    }
     */

    /*
    it("Greg's test") {
//      val inst = Person("", 34, List(1, 2, 3), Dog(3, "kibble")) // , Map("a"->1,"b"->2,"c"->3))
      val inst = PersonG(
        "",
        34,
//        List(Ball(5), Ball(6), Ball(7)),
//        List(Ball(5), Ball(6), Ball(7)),
//        List(Ball(5), Ball(6), Ball(7)),
//        Dog(3, "kibble"),
        List(CarG("Porsche"), CarG("Ferrari")),
        BallG(1)
//        Ball(2),
//        Some(Ball(3)),
//        Some(Ball(4))
//        Some(List(Ball(5), Ball(6), Ball(7))),
//        Some(List(Ball(5), Ball(6), Ball(7)))
      ) // , Map("a"->1,"b"->2,"c"->3))
      val sj = sjXmlCodecOf[PersonG](SJConfig.preferTypeHints)
      val xml = sj.toXml(inst)
      println(xml)
      val x = sj.fromXml(xml)
      println(x)
//      xml shouldEqual("""<dude><name>Bob</name><duration>34</duration></dude>""")
      //      js should matchJson("""{"name":"Bob","duration":34}""")
      //      sj.fromJson(js) shouldEqual (inst)
    }

    it("works") {
      val sj = sjXmlCodecOf[Invoice810]
      val x = sj.fromXml(foo)
//      println(x)
      val sj2 = sjCodecOf[Invoice810]
      val js = sj2.toJson(x)
      println("-------------")
      println(js)
//      println(x.toX12)
    }
     */
  }

  val foo = """<_DOC>
              |	<TYPE>DMS Invoice</TYPE>
              |	<recid>5662964326</recid>
              |	<invoiceno>INV-5028817</invoiceno>
              |	<invoicedate>20250603</invoicedate>
              |	<orderno>SO-2318909</orderno>
              |	<ordertype>3</ordertype>
              |	<orderdate>20250603</orderdate>
              |	<purchaseorderno>7897898</purchaseorderno>
              |	<cusno>270400</cusno>
              |	<division/>
              |	<department/>
              |	<shipdate>20250603</shipdate>
              |	<canceldate>20250605</canceldate>
              |	<collect/>
              |	<bolno>BOL-2247065</bolno>
              |	<iscreditmemo/>
              |	<miscamount>0.00</miscamount>
              |	<freightamount>0.00</freightamount>
              |	<totinvoiceamount>1183.950000000000</totinvoiceamount>
              |	<tottaxableamount>1183.950000000000</tottaxableamount>
              |	<totsalesamount>1183.950000000000</totsalesamount>
              |	<totcartons>0</totcartons>
              |	<totweight>0.0</totweight>
              |	<lineitemtotal>0</lineitemtotal>
              |
              |	<_CARRIER>
              |		<carrierid>1Day</carrierid>
              |		<carrierdesc>1 Day Delivery</carrierdesc>
              |	</_CARRIER>
              |
              |	<_MESSAGE>
              |		<TYPE>shippinginstructions1</TYPE>
              |		<message/>
              |	</_MESSAGE>
              |
              |
              |	<_MESSAGE>
              |		<TYPE>shippinginstructions2</TYPE>
              |		<message/>
              |	</_MESSAGE>
              |
              |	<_MESSAGE>
              |		<TYPE>comment1</TYPE>
              |		<message>7897898</message>
              |	</_MESSAGE>
              |
              |
              |	<_MESSAGE>
              |		<TYPE>comment2</TYPE>
              |		<message/>
              |	</_MESSAGE>
              |
              |
              |	<_MESSAGE>
              |		<TYPE>comment3</TYPE>
              |		<message/>
              |	</_MESSAGE>
              |
              |
              |	<_TERMS>
              |		<id>N21</id>
              |		<desc>21 days</desc>
              |		<duedays>21</duedays>
              |		<discountdays>0</discountdays>
              |		<discountpercent>0</discountpercent>
              |		<discountdate/>
              |		<datedue/>
              |	</_TERMS>
              |
              |
              |	<_CURRENCY>
              |		<currencycode>USD</currencycode>
              |		<currencyrate>1.0000000000000000</currencyrate>
              |	</_CURRENCY>
              |
              |
              |	<_CREDITMEMO>
              |		<origordtype>3</origordtype>
              |		<origordno>SO-2318909</origordno>
              |		<origorddate>20250603</origorddate>
              |		<applytono>0</applytono>
              |	</_CREDITMEMO>
              |
              |
              |	<_ADDRESS>
              |		<TYPE>billto</TYPE>
              |		<id>270400</id>
              |		<name>PFG Customized Mckinney</name>
              |		<add1>245 Castle Heights Avenue North</add1>
              |		<add2/>
              |		<add3></add3>
              |		<city>Lebanon</city>
              |		<state>TN</state>
              |		<zip>37087</zip>
              |		<country/>
              |		<contact/>
              |		<phone/>
              |		<fax/>
              |		<email/>
              |
              |
              |		<_USERDEF>
              |			<TYPE>user1</TYPE>
              |			<userdef/>
              |		</_USERDEF>
              |
              |
              |		<_USERDEF>
              |			<TYPE>user2</TYPE>
              |			<userdef/>
              |		</_USERDEF>
              |
              |
              |		<_USERDEF>
              |			<TYPE>user3</TYPE>
              |			<userdef/>
              |		</_USERDEF>
              |
              |
              |		<_USERDEF>
              |			<TYPE>user4</TYPE>
              |			<userdef/>
              |		</_USERDEF>
              |
              |
              |		<_USERDEF>
              |			<TYPE>user5</TYPE>
              |			<userdef/>
              |		</_USERDEF>
              |	</_ADDRESS>
              |
              |
              |	<_ADDRESS>
              |		<TYPE>shipto</TYPE>
              |		<id>7897898</id>
              |		<name>AlternateName</name>
              |		<add1>418 Power House DriveAddress2</add1>
              |		<add2/>
              |		<add3/>
              |		<city>McKinney</city>
              |		<state>TX</state>
              |		<zip>75071</zip>
              |		<country/>
              |		<contact/>
              |		<phone/>
              |		<fax/>
              |		<email/>
              |
              |
              |		<_USERDEF>
              |			<TYPE>user1</TYPE>
              |			<userdef/>
              |		</_USERDEF>
              |
              |
              |		<_USERDEF>
              |			<TYPE>user2</TYPE>
              |			<userdef/>
              |		</_USERDEF>
              |
              |
              |		<_USERDEF>
              |			<TYPE>user3</TYPE>
              |			<userdef/>
              |		</_USERDEF>
              |
              |
              |		<_USERDEF>
              |			<TYPE>user4</TYPE>
              |			<userdef/>
              |		</_USERDEF>
              |
              |
              |		<_USERDEF>
              |			<TYPE>user5</TYPE>
              |			<userdef/>
              |		</_USERDEF>
              |	</_ADDRESS>
              |
              |
              |	<_ADDRESS>
              |		<TYPE>shipfr</TYPE>
              |		<id>CHW</id>
              |		<name>Cockrell Hill Warehouse</name>
              |		<add1>1001 N. Cockrell Hill Road</add1>
              |		<add2/>
              |		<add3/>
              |		<city>Dallas</city>
              |		<state>TX</state>
              |		<zip>75211</zip>
              |		<country/>
              |		<contact/>
              |		<phone/>
              |		<fax/>
              |		<email/>
              |
              |
              |		<_USERDEF>
              |			<TYPE>user1</TYPE>
              |			<userdef/>
              |		</_USERDEF>
              |
              |
              |		<_USERDEF>
              |			<TYPE>user2</TYPE>
              |			<userdef/>
              |		</_USERDEF>
              |
              |
              |		<_USERDEF>
              |			<TYPE>user3</TYPE>
              |			<userdef/>
              |		</_USERDEF>
              |

              |		<_USERDEF>
              |			<TYPE>user4</TYPE>
              |			<userdef/>
              |		</_USERDEF>
              |
              |
              |		<_USERDEF>
              |			<TYPE>user5</TYPE>
              |			<userdef/>
              |		</_USERDEF>
              |	</_ADDRESS>
              |
              |
              |	<_ADDRESS>
              |		<TYPE>altadr</TYPE>
              |		<id>7897898</id>
              |		<name/>
              |		<add1/>
              |		<add2/>
              |		<add3/>
              |		<city/>
              |		<state/>
              |		<zip/>
              |		<country/>
              |		<contact/>
              |		<phone/>
              |		<fax/>
              |		<email/>
              |
              |
              |		<_USERDEF>
              |			<TYPE>user1</TYPE>
              |			<userdef/>
              |		</_USERDEF>
              |
              |
              |		<_USERDEF>
              |			<TYPE>user2</TYPE>
              |			<userdef/>
              |		</_USERDEF>
              |
              |
              |		<_USERDEF>
              |			<TYPE>user3</TYPE>
              |			<userdef/>
              |		</_USERDEF>
              |
              |
              |		<_USERDEF>
              |			<TYPE>user4</TYPE>
              |			<userdef/>
              |		</_USERDEF>
              |
              |
              |		<_USERDEF>
              |			<TYPE>user5</TYPE>
              |			<userdef/>
              |		</_USERDEF>
              |	</_ADDRESS>
              |
              |
              |	<_TAX>
              |		<taxsched/>
              |		<taxcode/>
              |		<taxableamount>1183.950000000000</taxableamount>
              |		<taxamt>0</taxamt>
              |		<taxpercent>0</taxpercent>
              |		<taxstate/>
              |	</_TAX>
              |
              |
              |	<_USERDEF>
              |		<TYPE>user1</TYPE>
              |		<userdef/>
              |	</_USERDEF>
              |
              |
              |	<_USERDEF>
              |		<TYPE>user2</TYPE>
              |		<userdef/>
              |	</_USERDEF>
              |
              |
              |	<_USERDEF>
              |		<TYPE>user3</TYPE>
              |		<userdef/>
              |	</_USERDEF>
              |
              |
              |	<_USERDEF>
              |		<TYPE>user4</TYPE>
              |		<userdef/>
              |	</_USERDEF>
              |
              |
              |	<_USERDEF>
              |		<TYPE>user5</TYPE>
              |		<userdef/>
              |	</_USERDEF>
              |
              |

              |	<_ITEM>
              |		<recid>6174159326</recid>
              |		<lineno>0.000000000000</lineno>
              |		<itemid>5900015</itemid>
              |		<cusitemid>71529</cusitemid>
              |		<itemdesc>Broc, Florets TF 4/3#</itemdesc>
              |		<itemdesc2/>
              |		<price>34.500000000000</price>
              |		<extendedprice>345.000000</extendedprice>
              |		<taxable/>
              |		<taxflag/>
              |		<extendedtaxamount/>
              |		<qtyord>10.000000000000</qtyord>
              |		<qtytoship>10.000000000000</qtytoship>
              |		<uom>CASE</uom>
              |		<requestdate>20250603</requestdate>
              |		<promisedate>20250605</promisedate>
              |		<requestedshipdate>20250605</requestedshipdate>
              |		<pickdate/>
              |		<shipdate/>
              |		<qtyreturntostk/>
              |		<reasoncd/>
              |
              |
              |		<_MESSAGE>
              |			<TYPE>comment1</TYPE>
              |			<message>7897898</message>
              |		</_MESSAGE>
              |
              |
              |		<_MESSAGE>
              |			<TYPE>comment2</TYPE>
              |			<message/>
              |		</_MESSAGE>
              |
              |
              |		<_MESSAGE>
              |			<TYPE>comment3</TYPE>
              |			<message/>
              |		</_MESSAGE>
              |
              |		<_USERDEF>
              |			<TYPE>user1</TYPE>
              |			<userdef/>
              |		</_USERDEF>
              |
              |
              |		<_USERDEF>
              |			<TYPE>user2</TYPE>
              |			<userdef/>
              |		</_USERDEF>
              |
              |
              |		<_USERDEF>
              |			<TYPE>user3</TYPE>
              |			<userdef>1</userdef>
              |		</_USERDEF>
              |
              |		<_USERDEF>
              |			<TYPE>user4</TYPE>
              |			<userdef>0</userdef>
              |		</_USERDEF>
              |
              |
              |		<_USERDEF>
              |			<TYPE>user5</TYPE>
              |			<userdef/>
              |		</_USERDEF>
              |
              |
              |		<_ITEMTAX>
              |			<taxableamt>0</taxableamt>
              |			<taxamount>0</taxamount>
              |			<taxsched/>
              |			<taxcd/>
              |		</_ITEMTAX>
              |	</_ITEM>
              |
              |
              |	<_ITEM>
              |		<recid>6174159327</recid>
              |		<lineno>1.000000000000</lineno>
              |		<itemid>2901890</itemid>
              |		<cusitemid>102204</cusitemid>
              |		<itemdesc>Cilantro, Chopped 3/8" TF 6/12oz</itemdesc>
              |		<itemdesc2/>
              |		<price>13.380000000000</price>
              |		<extendedprice>200.700000</extendedprice>
              |		<taxable/>
              |		<taxflag/>
              |		<extendedtaxamount/>
              |		<qtyord>15.000000000000</qtyord>
              |		<qtytoship>15.000000000000</qtytoship>
              |		<uom>CASE</uom>
              |		<requestdate>20250603</requestdate>
              |		<promisedate>20250605</promisedate>
              |		<requestedshipdate>20250605</requestedshipdate>
              |		<pickdate/>
              |		<shipdate/>
              |		<qtyreturntostk/>
              |		<reasoncd/>
              |
              |
              |		<_MESSAGE>
              |			<TYPE>comment1</TYPE>
              |			<message>7897898</message>
              |		</_MESSAGE>
              |
              |
              |		<_MESSAGE>
              |			<TYPE>comment2</TYPE>
              |			<message/>
              |		</_MESSAGE>
              |
              |
              |		<_MESSAGE>
              |			<TYPE>comment3</TYPE>
              |			<message/>
              |		</_MESSAGE>
              |
              |
              |		<_USERDEF>
              |			<TYPE>user1</TYPE>
              |			<userdef/>
              |		</_USERDEF>
              |
              |
              |		<_USERDEF>
              |			<TYPE>user2</TYPE>
              |			<userdef/>
              |		</_USERDEF>
              |
              |
              |		<_USERDEF>
              |			<TYPE>user3</TYPE>
              |			<userdef>2</userdef>
              |		</_USERDEF>
              |
              |
              |		<_USERDEF>
              |			<TYPE>user4</TYPE>
              |			<userdef>0</userdef>
              |		</_USERDEF>
              |
              |
              |		<_USERDEF>
              |			<TYPE>user5</TYPE>
              |			<userdef/>
              |		</_USERDEF>
              |
              |
              |		<_ITEMTAX>
              |			<taxableamt>0</taxableamt>
              |			<taxamount>0</taxamount>
              |			<taxsched/>
              |			<taxcd/>
              |		</_ITEMTAX>
              |	</_ITEM>
              |
              |
              |	<_ITEM>
              |		<recid>6174159328</recid>
              |		<lineno>2.000000000000</lineno>
              |		<itemid>2200280</itemid>
              |		<cusitemid>399867</cusitemid>
              |		<itemdesc>Lett, Blend, 70% 20% Rom Col Sep 4/5#</itemdesc>
              |		<itemdesc2/>
              |		<price>21.300000000000</price>
              |		<extendedprice>319.500000</extendedprice>
              |		<taxable/>
              |		<taxflag/>
              |		<extendedtaxamount/>
              |		<qtyord>15.000000000000</qtyord>
              |		<qtytoship>15.000000000000</qtytoship>
              |		<uom>CASE</uom>
              |		<requestdate>20250603</requestdate>
              |		<promisedate>20250605</promisedate>
              |		<requestedshipdate>20250605</requestedshipdate>
              |		<pickdate/>
              |		<shipdate/>
              |		<qtyreturntostk/>
              |		<reasoncd/>
              |
              |
              |		<_MESSAGE>
              |			<TYPE>comment1</TYPE>
              |			<message>7897898</message>
              |		</_MESSAGE>
              |
              |
              |		<_MESSAGE>
              |			<TYPE>comment2</TYPE>
              |			<message/>
              |		</_MESSAGE>
              |
              |		<_MESSAGE>
              |			<TYPE>comment3</TYPE>
              |			<message/>
              |		</_MESSAGE>
              |
              |
              |		<_USERDEF>
              |			<TYPE>user1</TYPE>
              |			<userdef/>
              |		</_USERDEF>
              |
              |
              |		<_USERDEF>
              |			<TYPE>user2</TYPE>
              |			<userdef/>
              |		</_USERDEF>
              |
              |
              |		<_USERDEF>
              |			<TYPE>user3</TYPE>
              |			<userdef>3</userdef>
              |		</_USERDEF>
              |
              |
              |		<_USERDEF>
              |			<TYPE>user4</TYPE>
              |			<userdef>0</userdef>
              |		</_USERDEF>
              |
              |
              |		<_USERDEF>
              |			<TYPE>user5</TYPE>
              |			<userdef/>
              |		</_USERDEF>
              |
              |
              |		<_ITEMTAX>
              |			<taxableamt>0</taxableamt>
              |			<taxamount>0</taxamount>
              |			<taxsched/>
              |			<taxcd/>
              |		</_ITEMTAX>
              |	</_ITEM>
              |
              |
              |	<_ITEM>
              |		<recid>6174159329</recid>
              |		<lineno>3.000000000000</lineno>
              |		<itemid>2200005</itemid>
              |		<cusitemid>446792</cusitemid>
              |		<itemdesc>Lett, Shred 1/8" 4/5#</itemdesc>
              |		<itemdesc2/>
              |		<price>21.250000000000</price>
              |		<extendedprice>318.750000</extendedprice>
              |		<taxable/>
              |		<taxflag/>
              |		<extendedtaxamount/>
              |		<qtyord>15.000000000000</qtyord>
              |		<qtytoship>15.000000000000</qtytoship>
              |		<uom>CASE</uom>
              |		<requestdate>20250603</requestdate>
              |		<promisedate>20250605</promisedate>
              |		<requestedshipdate>20250605</requestedshipdate>
              |		<pickdate/>
              |		<shipdate/>
              |		<qtyreturntostk/>
              |		<reasoncd/>
              |
              |
              |		<_MESSAGE>
              |			<TYPE>comment1</TYPE>
              |			<message>7897898</message>
              |		</_MESSAGE>
              |
              |
              |		<_MESSAGE>
              |			<TYPE>comment2</TYPE>
              |			<message/>
              |		</_MESSAGE>
              |
              |
              |		<_MESSAGE>
              |			<TYPE>comment3</TYPE>
              |			<message/>
              |		</_MESSAGE>
              |
              |		<_USERDEF>
              |			<TYPE>user1</TYPE>
              |			<userdef/>
              |		</_USERDEF>
              |
              |
              |		<_USERDEF>
              |			<TYPE>user2</TYPE>
              |			<userdef/>
              |		</_USERDEF>
              |
              |
              |		<_USERDEF>
              |			<TYPE>user3</TYPE>
              |			<userdef>4</userdef>
              |		</_USERDEF>
              |
              |
              |		<_USERDEF>
              |			<TYPE>user4</TYPE>
              |			<userdef>0</userdef>
              |		</_USERDEF>
              |
              |
              |		<_USERDEF>
              |			<TYPE>user5</TYPE>
              |			<userdef/>
              |		</_USERDEF>
              |
              |
              |		<_ITEMTAX>
              |			<taxableamt>0</taxableamt>
              |			<taxamount>0</taxamount>
              |			<taxsched/>
              |			<taxcd/>
              |		</_ITEMTAX>
              |	</_ITEM>
              |</_DOC>""".stripMargin
