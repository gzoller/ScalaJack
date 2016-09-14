package co.blocke.scalajack
package test

import org.scalatest.{ FunSpec, GivenWhenThen, BeforeAndAfterAll }
import org.scalatest.Matchers._
import scala.util.Try
import java.util.UUID
import java.time._

case class JavaAllTime(
  duration: Duration,
  instant:  Instant,
  localDT:  LocalDateTime,
  localD:   LocalDate,
  offsetDT: OffsetDateTime,
  offsetT:  OffsetTime,
  period:   Period,
  zonedDT:  ZonedDateTime
)

class JavaTimeSpec extends FunSpec with GivenWhenThen with BeforeAndAfterAll {
  describe("=========================\n| -- Java Time Tests -- |\n=========================") {
    describe("Read/render basic Java time objects") {
      it("Works") {
        val jat = JavaAllTime(
          Duration.parse("PT15S"),
          Instant.parse("2016-09-14T21:36:21.507Z"),
          LocalDateTime.parse("2016-09-14T16:36:21.513"),
          LocalDate.parse("2016-09-14"),
          OffsetDateTime.parse("2016-09-14T16:36:21.521-05:00"),
          OffsetTime.parse("16:36:21.522-05:00"),
          Period.ofWeeks(2),
          ZonedDateTime.parse("2016-09-14T16:36:21.522-05:00[America/Chicago]")
        )
        val js = ScalaJack().render(jat)
        js should equal("""{"duration":"PT15S","instant":"2016-09-14T21:36:21.507Z","localDT":"2016-09-14T16:36:21.513","localD":"2016-09-14","offsetDT":"2016-09-14T16:36:21.521-05:00","offsetT":"16:36:21.522-05:00","period":"P14D","zonedDT":"2016-09-14T16:36:21.522-05:00[America\/Chicago]"}""")
        ScalaJack().read[JavaAllTime](js) should equal(jat)
      }
    }
  }
}
