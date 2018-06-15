package co.blocke.scalajack

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

  def timestamp: Option[OffsetDateTime]

  def randomThing: Option[Either[List[Int], Map[String, Try[Long]]]]

}

class DependentTypeSpec extends FunSpec {

  val sj = ScalaJack(JsonFlavor())

  describe("Dependent types") {
    it("should work") {
      val t = typeOf[PourCoffeeType.type]
      println(t)

      implicit val ops: JsonOps[JValue] = Json4sOps
      val DeserializationSuccess(TypeTagged(envelope)) = sj.context.deserializerOf[Envelope].deserialize(Path.Root, ops.parse("""{"envelopeType": "PourCoffeeType", "payload": {"numberOfCups": 56}}"""))

      envelope.timestamp
      println(envelope.envelopeType)

      envelope.payload

      println(envelope)
    }
  }

}
