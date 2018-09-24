package co.blocke.scalajack
package json
package test.json4s

import java.time.OffsetDateTime

import co.blocke.scalajack.json.JsonFlavor
import org.json4s.JsonAST.JValue
import org.scalatest.FunSpec

import scala.util.Try

trait EnvelopeType {

  type Payload

}

object PourCoffeeType extends EnvelopeType {

  override type Payload = PourCoffee

}

case class PourCoffee(numberOfCups: Int)

trait Envelope {

  val envelopeType: EnvelopeType

  def payload: envelopeType.Payload

  val timestamp: Option[OffsetDateTime]

  //  def ts: timestamp.type

  def randomThing: Option[Either[List[Int], Map[String, Try[Long]]]]

}

class DependentTypeSpec() extends FunSpec {

  val sj = ScalaJack(JsonFlavor())

  describe("Dependent types") {
    it("should work") {
      val t = typeOf[PourCoffeeType.type]
      println(t)

      implicit val ops: JsonOps[JValue] = Json4sOps
      val DeserializationSuccess(TypeTagged(envelope)) = sj.context.deserializerOf[Envelope].deserialize(Path.Root, ops.parse("""{"envelopeType": "PourCoffeeType", "payload": {"numberOfCups": 56}}"""))

      val ts = envelope.timestamp
      println(ts)
      println(envelope.envelopeType)

      val pay = envelope.payload
      println(pay)

      val rt = envelope.randomThing
      println(rt)

      println(envelope)
    }
  }

}
