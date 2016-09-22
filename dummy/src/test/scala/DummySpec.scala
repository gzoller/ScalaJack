package co.blocke
package test

import org.scalatest.FunSpec
import org.scalatest.Matchers._

class SelfRefSpec extends FunSpec {
  describe("=====================\n| -- Dummy Tests -- |\n=====================") {
    it("Works?") {
      val d = Dummy(5)
      d.add(1) should be(6)
    }
  }
}