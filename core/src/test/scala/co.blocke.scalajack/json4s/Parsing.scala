package co.blocke.scalajack
package json4s

import TestUtil._
import munit._
import munit.internal.console
import org.json4s._
import co.blocke.scalajack.json4s.Json4sFlavor

class Parsing() extends FunSuite:

  val sj = co.blocke.scalajack.ScalaJack(Json4sFlavor())

  test("Null String value") {
    describe(
      "----------------------\n:  Parsing (Json4s)  :\n----------------------", Console.BLUE
    )
    assertEquals(sj.read[String](null), null)
  }

  test("Null (BSON null) String value") {
    assertEquals(sj.read[String](JNull), null)
  }

  test("Non-String value where String expected") {
    interceptMessage[co.blocke.scalajack.ScalaJackError]("Expected string here, not 'JInt(5)'"){
      sj.read[String](JInt(5))
    }
  }

  test("Null List value") {
    assertEquals(sj.read[List[Int]](null), null)
  }

  test("Null (BSON null) List value") {
    assertEquals(sj.read[List[Int]](JNull), null)
  }

  test("Non-List value where List expected") {
    interceptMessage[co.blocke.scalajack.ScalaJackError]("Expected list here, not 'JInt(5)'"){
      sj.read[List[Int]](JInt(5))
    }
  }

  test("Null tuple value") {
    assertEquals(sj.read[(Int, Int)](null), null)
  }

  test("Null (BSON null) tuple value") {
    assertEquals(sj.read[(Int, Int)](JNull), null)
  }

  test("Non-tuple value where tuple expected") {
    interceptMessage[co.blocke.scalajack.ScalaJackError]("Expected tuple (list) here, not 'JInt(5)'"){
      sj.read[(Int, Int)](JInt(5))
    }
  }

  test("Null Map value") {
    assertEquals(sj.read[Map[String, Int]](null), null)
  }

  test("Null (BSON null) Map value") {
    assertEquals(sj.read[Map[String, Int]](JNull), null)
  }

  test("Non-Map value where Map expected") {
    interceptMessage[co.blocke.scalajack.ScalaJackError]("Expected map here, not 'JInt(5)'"){
      sj.read[Map[String, Int]](JInt(5))
    }
  }

  test("Null object value") {
    assertEquals(sj.read[Person](null), null)
  }

  test("Null (BSON null) object value") {
    assertEquals(sj.read[Person](JNull), null)
  }

  test("Non-Boolean value where Boolean expected") {
    interceptMessage[co.blocke.scalajack.ScalaJackError]("Expected boolean here, not 'JInt(5)'"){
      sj.read[Boolean](JInt(5))
    }
  }

  test("Non-Number value where Number expected") {
    interceptMessage[co.blocke.scalajack.ScalaJackError]("Expected number here, not 'JString(x)'"){
      sj.read[Int](new JString("x"))
    }
  }

  test("Attempt to scan for type hint on a non-object") {
    interceptMessage[co.blocke.scalajack.ScalaJackError]("Expected object here, not 'JInt(5)'"){
      sj.read[Address](JInt(5))
    }
  }

  test("Attempt to resolve type members on a non-object") {
    interceptMessage[co.blocke.scalajack.ScalaJackError]("Expected object here, not 'JInt(5)'"){
      sj.read[Envelope[FancyBody]](JInt(5))
    }
  }