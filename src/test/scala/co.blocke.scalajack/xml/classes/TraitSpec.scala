package co.blocke.scalajack
package xml
package classes

import ScalaJack.*
import co.blocke.scala_reflection.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*
import org.scalatest.*
import TestUtil.*

class TraitSpec() extends AnyFunSpec:
  opaque type phone = String

  describe(colorString("-------------------------------\n:         Trait Tests         :\n-------------------------------", Console.YELLOW)) {
    it("Sealed trait with case objects and case classes must work") {
      val inst = TraitHolder(Start, Fish("Beta", false), Miami(101.1), CityRouteImpl(99))
      val sj = sjXmlCodecOf[TraitHolder]
      val x = sj.toXml(inst)
      x should equal("""<TraitHolder><a>Start</a><b><Fish><species>Beta</species><freshwater>false</freshwater></Fish></b><c><Miami><temp>101.1</temp></Miami></c><d><CityRoute><numStreets>99</numStreets></CityRoute></d></TraitHolder>""")
      val re = sj.fromXml(x)
      re.a shouldEqual (inst.a)
      re.b shouldEqual (inst.b)
      (re.c.asInstanceOf[Miami].temp == inst.c.asInstanceOf[Miami].temp) shouldEqual (true)
      (re.d.asInstanceOf[CityRoute].numStreets == inst.d.asInstanceOf[CityRoute].numStreets) shouldEqual (true)
    }
    it("Complex trait relationships must work") {
      val inst: ComplexPerson = Employee(Painter(5, Sports(1.2, 'Z')), Car(4))
      val sj = sjXmlCodecOf[ComplexPerson](SJConfig.preferTypeHints)
      val x = sj.toXml(inst)
      x should equal("""<Employee><who><Painter><instrument>5</instrument><effort><Sports><thing1>1.2</thing1><thing2>Z</thing2></Sports></effort></Painter></who><org><Car><passengers>4</passengers></Car></org></Employee>""")
      sj.fromXml(x) shouldEqual (inst)
    }
    it("Complex test with nested trait and empty unique field") {
      val a = L0A(5, None, true)
      val b = L0B(Some("wow"), "abc", None)
      val c = L0C(Some(3), List(1, 2, 3))
      val r = L1R("blather", None, a)
      val s = L1S(123L, "Miguel")
      val q = L1Q("aaa", 100, b)
      val xb = L1X("bbb", 99, c)
      val inst = ComplexHolder(r, s, q, xb)
      val sj = sjXmlCodecOf[ComplexHolder]
      val x = sj.toXml(inst)
      x should equal(
        """<ComplexHolder><c1><L1R><blather>blather</blather><l0><L0A><x>5</x><y>true</y></L0A></l0></L1R></c1><c2><L1S><id>123</id><nombre>Miguel</nombre></L1S></c2><c3><L1Q><name>aaa</name><age>100</age><l0><L0B><name>wow</name><id>abc</id></L0B></l0></L1Q></c3><c4><L1X><name>bbb</name><age>99</age><l0><L0C><id>3</id><extra><locItem>1</locItem><locItem>2</locItem><locItem>3</locItem></extra></L0C></l0></L1X></c4></ComplexHolder>"""
      )
      sj.fromXml(x) shouldEqual (inst)
    }
    it("Self-referencing must work") {
      val inst = OuterImpl("foo", 55, List(OuterImpl("bar", 100, Nil)))
      val sj = sjXmlCodecOf[Outer]
      val x = sj.toXml(inst)
      x should equal(
        """<OuterImpl><name>foo</name><num>55</num><stuff><OuterImpl><name>bar</name><num>100</num><stuff/></OuterImpl></stuff></OuterImpl>"""
      )
      sj.fromXml(x) shouldEqual (inst)
    }
  }
