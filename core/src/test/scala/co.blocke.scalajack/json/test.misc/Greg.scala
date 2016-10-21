package co.blocke.scalajack
package json.test.misc

import org.scalatest.{ FunSpec, Matchers }
import java.util.UUID
import scala.reflect.runtime.universe.typeOf

import scala.util._
import java.util.UUID

case class Amorphous(
  thing: Any
)
case class Small(num: Int)
case class UUID_VC(underlying: UUID) extends AnyVal

class Greg() extends FunSpec with Matchers {

  val sj = ScalaJack()

  describe("---------------------------\n:  Try and Capture Tests  :\n---------------------------") {
    it("Map key - Option") {
      val all = List(
        Amorphous(true),
        Amorphous("blather"),
        Amorphous(1.234),
        Amorphous(List(1, 2, 3)),
        Amorphous(Map("a" -> 1, "b" -> 2)),
        Amorphous(null),
        Amorphous(Small(99))
      )

      println(sj.render(all))
    }
    it("Tuples") {
      println("------------------")
      val x = UUID_VC(UUID.randomUUID)
      val js = sj.render(x)
      println(sj.read[UUID_VC](js))
    }
  }
}
