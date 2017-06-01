package co.blocke.scalajack

import org.scalatest.{ BeforeAndAfterAll, FunSpec, GivenWhenThen }
import org.scalatest.Matchers._
import scala.reflect.runtime.universe.typeOf

import java.time.OffsetDateTime

case class Message[T <: MessagePayload](
    id:        String,
    timestamp: OffsetDateTime,
    metadata:  Map[String, Any],
    payload:   T
) {
  type `type` = T
}

object Message {

  def apply[T <: MessagePayload](metadata: Map[String, Any], payload: T): Message[T] =
    Message("abc123", OffsetDateTime.now, metadata, payload)

}

// Marker trait
sealed trait MessagePayload

// Sample kinds of specific payload sub-types
trait OrganicPayload extends MessagePayload
trait InorganicPayload extends MessagePayload

// Default "no-op" payload for when we can't parse the given message's payload content
case class UnknownPayload() extends MessagePayload
// case class UnknownPayload(kind: String) extends MessagePayload

trait Pet extends OrganicPayload {
  val numLegs: Int
  val food: String
}

// Concrete payload classes with default values
case class Cat(numLegs: Int = 4, food: String = "milk") extends Pet
case class Dog(numLegs: Int = 4, food: String = "kibbles", bites: Boolean = false) extends Pet

class Greg extends FunSpec with GivenWhenThen with BeforeAndAfterAll {

  val sj = ScalaJack().parseOrElse(typeOf[MessagePayload] -> typeOf[UnknownPayload])

  describe("------------------\n:  Either Tests  :\n------------------") {
    it("Left - two class types") {
      // val m = Message[OrganicPayload](Map.empty[String, Any], Cat())
      // val js = sj.render(m)
      // println(js)

      val bogus = """{"id":"abc123","timestamp":"2017-05-31T13:00:07.391-05:00","metadata":{},"payload":{"_hint":"co.blocke.scalajack.Fish","numLegs":4,"food":"milk"}}"""
      // val bogus = """{"type":"co.blocke.scalajack.Fish","id":"abc123","timestamp":"2017-05-31T13:00:07.391-05:00","metadata":{},"payload":{"numLegs":4,"food":"milk"}}"""
      // try {
      val x = sj.read[Message[MessagePayload]](bogus)
      println(x)
      // } catch { case e: Exception => System.out.println("oops") }
    }
  }
}
