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
    it("Simple case class must work (with field renaming)") {
//      val inst = Person("", 34, List(1, 2, 3), Dog(3, "kibble")) // , Map("a"->1,"b"->2,"c"->3))
      val inst = Person(
        "",
        34,
//        List(Ball(5), Ball(6), Ball(7)),
//        List(Ball(5), Ball(6), Ball(7)),
//        List(Ball(5), Ball(6), Ball(7)),
//        Dog(3, "kibble"),
        Ball(1),
        List(Car("Porsche"), Car("Ferrari"))
//        Ball(2),
//        Some(Ball(3)),
//        Some(Ball(4))
//        Some(List(Ball(5), Ball(6), Ball(7))),
//        Some(List(Ball(5), Ball(6), Ball(7)))
      ) // , Map("a"->1,"b"->2,"c"->3))
      val sj = sjXmlCodecOf[Person](SJConfig.preferTypeHints)
      val xml = sj.toXml(inst)
      println(xml)
      val x = sj.fromXml(xml)
      println(x)
//      xml shouldEqual("""<dude><name>Bob</name><duration>34</duration></dude>""")
      //      js should matchJson("""{"name":"Bob","duration":34}""")
      //      sj.fromJson(js) shouldEqual (inst)
    }

    /*
<dude>
	<name/>
	<duration>34</duration>
	<normal>   <- Normal/canonical with xmlEntryLabel (item)
		<item>
			<Ball>
				<size>5</size>
			</Ball>
		</item>
		<item>
			<Ball>
				<size>6</size>
			</Ball>
		</item>
		<item>
			<Ball>
				<size>7</size>
			</Ball>
		</item>
	</normal>
	<Ball>
		<size>5</size>
	</Ball>
	<Ball>
		<size>6</size>
	</Ball>
	<Ball>
		<size>7</size>
	</Ball>
	<Ball>
		<size>5</size>
	</Ball>
	<Ball>
		<size>6</size>
	</Ball>
	<Ball>
		<size>7</size>
	</Ball>
	<pet>
		<Dog>
			<legs>3</legs>
			<food>kibble</food>
		</Dog>
	</pet>
	<Ball>
		<size>1</size>
	</Ball>
	<Ball>
		<size>2</size>
	</Ball>
</dude>
     */

    /*
    it("works") {
      val sj = sjXmlCodecOf[Invoice810]
      val x = sj.fromXml(foo)
      println(x)
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
              |		<messsage>7897898</messsage>
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
              |			<messsage>7897898</messsage>
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
              |			<messsage>7897898</messsage>
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
              |			<messsage>7897898</messsage>
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
              |			<messsage>7897898</messsage>
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
