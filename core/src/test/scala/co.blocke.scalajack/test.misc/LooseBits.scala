package co.blocke.scalajack
package test
package misc

import org.scalatest.{ FunSpec, GivenWhenThen, BeforeAndAfterAll }
import org.scalatest.Matchers._
import scala.util.Try
import java.util.UUID

class LooseBits extends FunSpec with GivenWhenThen with BeforeAndAfterAll {

  val sj = ScalaJack()

  describe("-------------------------\n:  Uncategorized Tests  :\n-------------------------") {
    // it("Non-string class member name (failure case)") {
    //   val js = """{[1,2]:"Fred","big":12L}"""
    //   sj.read[View1](js)
    // }
  }
}
