package co.blocke.scalajack
package json4s

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.json4s._
import scala.reflect.runtime.universe._

class Custom extends AnyFunSpec with Matchers {

  val sj = ScalaJack(Json4sFlavor())

  describe(
    "---------------------------\n:  Custom Tests (Json4s) :\n---------------------------"
  ) {
      it("parse()") {
        val p = sj.parse(JInt(5))
        p.isInstanceOf[Json4sParser] should be(true)
      }
      it("allowPermissivePrimitives()") {
        the[ScalaJackError] thrownBy sj
          .allowPermissivePrimitives() should have message "Permissive primitives not supported for Json4s"
      }
      it("parseOrElse()") {
        val sj2 = sj.parseOrElse(typeOf[Address] -> typeOf[DefaultAddress])
        val js4s =
          JObject(
            List("_hint" -> JString("unknown"), "postalCode" -> JString("12345"))
          )
        sj2.read[Address](js4s) should be(DefaultAddress("12345"))
      }
      it("withAdapters()") {
        val sj2 = sj.withAdapters(PhoneAdapter)
        val dbo = JObject(
          List("name" -> JString("Fred"), "phone" -> JString("123-456-7890"))
        )
        sj2.read[Employee](dbo) should equal(Employee("Fred", "1234567890"))
      }
      it("withDefaultHint()") {
        val sj2 = ScalaJack(Json4sFlavor()).withDefaultHint("kind")
        val dbo = new JObject(
          List(
            "kind" -> JString("co.blocke.scalajack.json4s.USDemographic"),
            "age" -> JInt(34),
            "address" -> JObject(
              List(
                "kind" -> JString("co.blocke.scalajack.json4s.USAddress"),
                "street" -> JString("123 Main"),
                "city" -> JString("New York"),
                "state" -> JString("NY"),
                "postalCode" -> JString("39822")
              )
            )
          )
        )
        sj2.read[Demographic](dbo) should equal(
          USDemographic(34, USAddress("123 Main", "New York", "NY", "39822"))
        )
      }
      it("withHints()") {
        val sj2 = ScalaJack(Json4sFlavor()).withHints(
          typeOf[Address] -> "addr_kind",
          typeOf[Demographic] -> "demo"
        )
        val dbo = new JObject(
          List(
            "demo" -> JString("co.blocke.scalajack.json4s.USDemographic"),
            "age" -> JInt(34),
            "address" -> JObject(
              List(
                "addr_kind" -> JString("co.blocke.scalajack.json4s.USAddress"),
                "street" -> JString("123 Main"),
                "city" -> JString("New York"),
                "state" -> JString("NY"),
                "postalCode" -> JString("39822")
              )
            )
          )
        )
        sj2.read[Demographic](dbo) should equal(
          USDemographic(34, USAddress("123 Main", "New York", "NY", "39822"))
        )
      }
      // Other custom configs are already covered in other tests elsewhere
    }
}
