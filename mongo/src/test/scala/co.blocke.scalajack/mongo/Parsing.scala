package co.blocke.scalajack
package mongo

import org.bson.types.ObjectId
import org.bson._
import org.scalatest.matchers.should._
import org.scalatest.funspec.AnyFunSpec

class Parsing extends AnyFunSpec with Matchers {

  val sj = ScalaJack(MongoFlavor())

  describe(
    "-----------------------\n:  Parsing (MongoDB)  :\n-----------------------"
  ) {
      it("Null String value") {
        sj.read[String](null) should be(null)
      }
      it("Null (BSON null) String value") {
        sj.read[String](new BsonNull()) should be(null)
      }
      it("Non-String value where String expected") {
        the[ScalaJackError] thrownBy sj.read[String](new BsonInt32(5)) should have message "Expected string here, not 'BsonInt32{value=5}'"
      }
      it("Null List value") {
        sj.read[List[Int]](null) should be(null)
      }
      it("Null (BSON null) List value") {
        sj.read[List[Int]](new BsonNull()) should be(null)
      }
      it("Non-List value where List expected") {
        the[ScalaJackError] thrownBy sj.read[List[Int]](new BsonInt32(5)) should have message "Expected list here, not 'BsonInt32{value=5}'"
      }
      it("Null tuple value") {
        sj.read[(Int, Int)](null) should be(null)
      }
      it("Null (BSON null) tuple value") {
        sj.read[(Int, Int)](new BsonNull()) should be(null)
      }
      it("Non-tuple value where tuple expected") {
        the[ScalaJackError] thrownBy sj.read[(Int, Int)](new BsonInt32(5)) should have message "Expected tuple (list) here, not 'BsonInt32{value=5}'"
      }
      it("Null Map value") {
        sj.read[Map[String, Int]](null) should be(null)
      }
      it("Null (BSON null) Map value") {
        sj.read[Map[String, Int]](new BsonNull()) should be(null)
      }
      it("Non-Map value where Map expected") {
        the[ScalaJackError] thrownBy sj.read[Map[String, Int]](new BsonInt32(5)) should have message "Expected document (map) here, not 'BsonInt32{value=5}'"
      }
      it("Null object value") {
        sj.read[Person](null) should be(null)
      }
      it("Null (BSON null) object value") {
        sj.read[Person](new BsonNull()) should be(null)
      }
      it("Non-Boolean value where Boolean expected") {
        the[ScalaJackError] thrownBy sj.read[Boolean](new BsonInt32(5)) should have message "Expected boolean here, not 'BsonInt32{value=5}'"
      }
      it("Non-Number value where Number expected") {
        the[ScalaJackError] thrownBy sj.read[Int](new BsonString("x")) should have message "Expected number here, not 'BsonString{value='x'}'"
      }
      it("Attempt to scan for type hint on a non-object") {
        the[ScalaJackError] thrownBy sj.read[PetAnimal](new BsonInt32(5)) should have message "Expected document here, not 'BsonInt32{value=5}'"
      }
      it("Attempt to resolve type members on a non-object") {
        the[ScalaJackError] thrownBy sj.read[Envelope[FancyBody]](
          new BsonInt32(5)
        ) should have message "Expected document (object) here, not 'BsonInt32{value=5}'"
      }
      it("Null ObjectId value") {
        sj.read[ObjectId](null) should be(null)
      }
      it("Null (BSON null) ObjectId value") {
        sj.read[ObjectId](new BsonNull()) should be(null)
      }
      it("Non-ObjectId value where ObjectId expected") {
        the[ScalaJackError] thrownBy sj.read[ObjectId](new BsonInt32(5)) should have message "Expected ObjectId here, not 'BsonInt32{value=5}'"
      }
    }
}
