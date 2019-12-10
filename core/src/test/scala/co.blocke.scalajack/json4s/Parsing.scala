package co.blocke.scalajack
package json4s

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should._
import org.json4s._

class Parsing extends AnyFunSpec with Matchers {

  val sj = ScalaJack(Json4sFlavor())

  describe(
    "-----------------------\n:  Parsing (MongoDB)  :\n-----------------------"
  ) {
      it("Null String value") {
        sj.read[String](null) should be(null)
      }
      it("Null (BSON null) String value") {
        sj.read[String](JNull) should be(null)
      }
      it("Non-String value where String expected") {
        the[ScalaJackError] thrownBy sj.read[String](JInt(5)) should have message "Expected string here, not 'JInt(5)'"
      }
      it("Null List value") {
        sj.read[List[Int]](null) should be(null)
      }
      it("Null (BSON null) List value") {
        sj.read[List[Int]](JNull) should be(null)
      }
      it("Non-List value where List expected") {
        the[ScalaJackError] thrownBy sj.read[List[Int]](JInt(5)) should have message "Expected list here, not 'JInt(5)'"
      }
      it("Null tuple value") {
        sj.read[(Int, Int)](null) should be(null)
      }
      it("Null (BSON null) tuple value") {
        sj.read[(Int, Int)](JNull) should be(null)
      }
      it("Non-tuple value where tuple expected") {
        the[ScalaJackError] thrownBy sj.read[(Int, Int)](JInt(5)) should have message "Expected tuple (list) here, not 'JInt(5)'"
      }
      it("Null Map value") {
        sj.read[Map[String, Int]](null) should be(null)
      }
      it("Null (BSON null) Map value") {
        sj.read[Map[String, Int]](JNull) should be(null)
      }
      it("Non-Map value where Map expected") {
        the[ScalaJackError] thrownBy sj.read[Map[String, Int]](JInt(5)) should have message "Expected map here, not 'JInt(5)'"
      }
      it("Null object value") {
        sj.read[Person](null) should be(null)
      }
      it("Null (BSON null) object value") {
        sj.read[Person](JNull) should be(null)
      }
      it("Non-Boolean value where Boolean expected") {
        the[ScalaJackError] thrownBy sj.read[Boolean](JInt(5)) should have message "Expected boolean here, not 'JInt(5)'"
      }
      it("Non-Number value where Number expected") {
        the[ScalaJackError] thrownBy sj.read[Int](new JString("x")) should have message "Expected number here, not 'JString(x)'"
      }
      it("Attempt to scan for type hint on a non-object") {
        the[ScalaJackError] thrownBy sj.read[Address](JInt(5)) should have message "Expected object here, not 'JInt(5)'"
      }
      it("Attempt to resolve type members on a non-object") {
        the[ScalaJackError] thrownBy sj.read[Envelope[FancyBody]](JInt(5)) should have message "Expected object here, not 'JInt(5)'"
      }
    }
}
