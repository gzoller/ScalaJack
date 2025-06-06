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
      val inst = Person("Bob", 34, List(Ball(5),Ball(6),Ball(7)))//, Map("a"->1,"b"->2,"c"->3))
      val sj = sjXmlCodecOf[Person]
      val xml = sj.toXml(inst)
      println(xml)
      val x = sj.fromXml(xml)
      println(x)
//      xml shouldEqual("""<dude><name>Bob</name><duration>34</duration></dude>""")
      //      js should matchJson("""{"name":"Bob","duration":34}""")
      //      sj.fromJson(js) shouldEqual (inst)
    }
  }