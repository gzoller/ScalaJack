package co.blocke.scalajack
package json.misc

import org.scalatest.funspec.AnyFunSpec

import scala.io.Source

case class PersonRecord(
    id:         Long,
    first_name: String,
    last_name:  String,
    email:      String,
    gender:     String,
    ip_address: String)

class LargeJson extends AnyFunSpec {

  val sj = ScalaJack()

  describe(
    "---------------------\n:  Large JSON Test  :\n---------------------"
  ) {
      it("Read and match") {
        val json = Source
          .fromInputStream(classOf[LargeJson].getResourceAsStream("/large.json"))
          .mkString
        sj.read[List[PersonRecord]](json)
      }
    }

}
