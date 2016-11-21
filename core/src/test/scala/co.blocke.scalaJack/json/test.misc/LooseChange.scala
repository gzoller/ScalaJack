package co.blocke.scalajack
package json.test.misc

import org.scalatest.{ FunSpec, GivenWhenThen, BeforeAndAfterAll }
import org.scalatest.Matchers._
import scala.util.Try
import java.util.UUID
import scala.reflect.runtime.universe.typeOf
import typeadapter.CaseClassTypeAdapter

class LooseChange extends FunSpec with GivenWhenThen with BeforeAndAfterAll {

  val sj = ScalaJack()

  describe("-----------------------\n:  Lose Change Tests  :\n-----------------------") {
    it("Bijective compose") {
      val bi1 = BijectiveFunction[Int, Boolean]((i: Int) => if (i > 3) true else false, (b: Boolean) => if (b) 3 else 0)
      val bi2 = BijectiveFunction[String, Int]((s: String) => s.length, (i: Int) => "x" * i)
      val bi3 = bi1.compose[String](bi2)
      bi3("blather") should be(true)
    }
    it("Bijective double inversion") {
      val bi = BijectiveFunction[String, Int]((s: String) => s.length, (i: Int) => "x" * i)
      val x = bi.inverse
      x(3) should be("xxx")
      val y = x.inverse
      y("foo") should be(3)
    }
    it("Can fid collection annotation on case class") {
      val adapter = sj.context.typeAdapter(typeOf[DefaultOpt]).asInstanceOf[CaseClassTypeAdapter[_]]
      adapter.collectionName should be(Some("myDefaults"))
      adapter.fieldMembers.head.dbKeyIndex should be(Some(0))
    }
  }
}
