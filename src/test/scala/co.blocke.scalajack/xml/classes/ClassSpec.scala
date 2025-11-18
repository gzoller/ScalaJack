package co.blocke.scalajack
package xml
package classes

import ScalaJack.*
import co.blocke.scala_reflection.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.*
import co.blocke.scalajack.json.SampleClass
import TestUtil.*

class ClassSpec() extends AnyFunSpec:
  opaque type phone = String

  describe(colorString("-------------------------------\n:      XML Class Tests        :\n-------------------------------", Console.YELLOW)) {
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
    it("Non-constructor fields of class must work") {
      val inst = Parent(99, List("x", "y"))
      inst.hidden_=(true)
      inst.nope_=(false)
      inst.foo = "we'll see"
      val sj = sjXmlCodecOf[Parent](SJConfig.writeNonConstructorFields)
      val x = sj.toXml(inst)
      x should equal("""<Parent><phase>99</phase><stuff><one>x</one><one>y</one></stuff><foo>we'll see</foo><hidden>true</hidden></Parent>""")
      val re = sj.fromXml(x)
      re.phase shouldEqual (inst.phase)
      re.stuff shouldEqual (inst.stuff)
      re.foo shouldEqual (inst.foo)
      re.hidden shouldEqual (inst.hidden)
    }
    it("Inherited class must work") {
      val inst = Child("Bob", 34, 3)
      val sj = sjXmlCodecOf[Child]
      val x = sj.toXml(inst)
      x should equal("""<Child><name>Bob</name><age>34</age><phase>3</phase></Child>""")
      sj.fromXml(x) shouldEqual (inst)
    }
    it("Block non-constructor fields of class must work") {
      val inst = Parent(99, List("x", "y"))
      inst.hidden_=(true)
      inst.nope_=(false)
      val sj = sjXmlCodecOf[Parent]
      val x = sj.toXml(inst)
      x should equal("""<Parent><phase>99</phase><stuff><one>x</one><one>y</one></stuff></Parent>""")
      val re = sj.fromXml(x)
      re.phase shouldEqual (inst.phase)
      re.stuff shouldEqual (inst.stuff)
      re.foo shouldEqual ("ok")
      re.hidden shouldEqual (false)
    }
    it("Sealed abstract class with case objects and case classes must work") {
      val inst = AbstractClassHolder(Start2, Fish2("Beta", false), Miami2(101.1))
      val sj = sjXmlCodecOf[AbstractClassHolder]
      val x = sj.toXml(inst)
      x should equal("""<AbstractClassHolder><a>Start2</a><b><Fish2><species>Beta</species><freshwater>false</freshwater></Fish2></b><c><Miami2><temp>101.1</temp></Miami2></c></AbstractClassHolder>""")
      val re = sj.fromXml(x)
      re.a shouldEqual (inst.a)
      re.b shouldEqual (inst.b)
      (re.c.asInstanceOf[Miami2].temp == inst.c.asInstanceOf[Miami2].temp) shouldEqual (true)
    }
    it("Parameterized sealed abstract class must work") {
      val inst = AbstractClassHolder2(Thing2(15L, "wow"))
      val sj = sjXmlCodecOf[AbstractClassHolder2[Long]]
      val x = sj.toXml(inst)
      x should equal("""<AbstractClassHolder2><a><Thing2><t>15</t><s>wow</s></Thing2></a></AbstractClassHolder2>""")
      val re = sj.fromXml(x)
      re.a.asInstanceOf[Thing2[Long]].t shouldEqual (15L)
      re.a.asInstanceOf[Thing2[Long]].s shouldEqual ("wow")
    }
    it("Top-level abstract class must work") {
      val inst: AThing[Long] = Thing2(99L, "ok")
      val sj = sjXmlCodecOf[AThing[Long]]
      val x = sj.toXml(inst)
      x should equal("""<Thing2><t>99</t><s>ok</s></Thing2>""")
      val re = sj.fromXml(x)
      re.asInstanceOf[Thing2[Long]].t shouldEqual (99L)
      re.asInstanceOf[Thing2[Long]].s shouldEqual ("ok")
    }
    it("Self-referencing class must work (bonus: parameterized self-referencing class)") {
      val inst = Empl("abc123", 5, Empl("xyz11", -1, null, Nil), List(Empl("tru777", 0, null, Nil), Empl("pop9", 9, null, Nil)))
      val sj = sjXmlCodecOf[Empl[Int]]
      val x = sj.toXml(inst)
      x should equal(
        """<Empl><id>abc123</id><data>5</data><boss><Empl><id>xyz11</id><data>-1</data><boss>null</boss><coworkers/></Empl></boss><coworkers><one><Empl><id>tru777</id><data>0</data><boss>null</boss><coworkers/></Empl></one><one><Empl><id>pop9</id><data>9</data><boss>null</boss><coworkers/></Empl></one></coworkers></Empl>"""
      )
      sj.fromXml(x) shouldEqual (inst)
    }
    it("Value classes must work") {
      val inst = ValueClassHolder(123, Email("abc@foom.com"))
      val sj = sjXmlCodecOf[ValueClassHolder]
      val x = sj.toXml(inst)
      x should equal(
        """<ValueClassHolder><id>123</id><email>abc@foom.com</email></ValueClassHolder>"""
      )
      sj.fromXml(x) shouldEqual (inst)

    }
    it("Java classes must work") {
      val inst = new SampleClass()
      inst.setName("John Doe")
      inst.setAge(45)
      inst.setAddress("123 Main St")
      val sj = sjXmlCodecOf[SampleClass]
      val x = sj.toXml(inst)
      x should equal("""<SampleClass><address>123 Main St</address><name>John Doe</name></SampleClass>""")
      val re = sj.fromXml(x)
      re.getName shouldEqual ("John Doe")
      re.getAge shouldEqual (0)
      re.getAddress shouldEqual ("123 Main St")
    }
    it("Abstract class value is null") {
      val inst = AbstractClassHolder(null, null, null)
      val sj = sjXmlCodecOf[AbstractClassHolder]
      val x = sj.toXml(inst)
      x should equal("""<AbstractClassHolder><a>null</a><b>null</b><c>null</c></AbstractClassHolder>""")
      val re = sj.fromXml(x)
      re.a shouldEqual (null)
      re.b shouldEqual (null)
      re.c shouldEqual (null)
    }

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
     */

    it("works") {
//      println(foo)
      val sj = sjXmlCodecOf[Invoice810]
      val x = sj.fromXml(foo)
//      println(x)
      val sj2 = sjCodecOf[Invoice810]
      val js = sj2.toJson(x)
      println("-------------")
//      println(js)
//      println(x.toX12)
    }
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
