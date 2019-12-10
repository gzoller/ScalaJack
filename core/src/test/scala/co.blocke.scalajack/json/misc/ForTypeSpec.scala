package co.blocke.scalajack
package json.misc

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ForTypeSpec extends AnyFunSpec with Matchers {

  val sj = ScalaJack()

  describe("-------------------\n:  ForType Tests  :\n-------------------") {
    it("Normal forType[T] works (read/render)") {
      val ft = sj.forType[List[Int]]
      val js = ft.render(List(1, 2, 3))
      js should be("[1,2,3]")
      ft.read(js) should be(List(1, 2, 3))
    }
    it("After forType, normal read/render works") {
      val ft = sj.forType[List[Int]]
      val js = ft.render[List[Boolean]](List(true, false, true))
      js should be("[true,false,true]")
      ft.read[List[Boolean]](js) should be(List(true, false, true))
    }
    it("After forType, another forType works works") {
      val ft = sj.forType[List[Int]].forType[String]
      val js = ft.render("Foom")
      js should be("\"Foom\"")
      ft.read(js) should be("Foom")
    }
  }
}
