package co.blocke.scalajack
package csv
package test

import org.scalatest.{ FunSpec, Matchers }
// import scala.reflect.runtime.universe.Type
// import java.util.UUID

class CSVTests() extends FunSpec with Matchers {

  val sj = ScalaJack(CSVFlavor())

  describe("---------------\n:  CSV Tests  :\n---------------") {
    /*
    describe("Primitives (non-null):") {
      it("Writes out basic Scala primitive types") {
        val inst = BasicScala(BigDecimal(123.45), BigInt(123), true, 64.asInstanceOf[Byte],
          'Z', 12.34, Size.Large, 12.34F, 5L, 5.asInstanceOf[Short], "wow", UUID.fromString("54cab778-7b9e-4b07-9d37-87b97a011e55"))
        val csv = sj.render(inst)
        assertResult("""123.45,123,true,64,Z,12.34,Large,12.34,5,5,wow,54cab778-7b9e-4b07-9d37-87b97a011e55""") { csv }
        assertResult(inst) {
          sj.read[BasicScala](csv)
        }
      }
      it("Handles Any type") {
        val inst = HasAny(true, "hey, you", 34.56)
        val csv = sj.render(inst)
        assertResult("""true,"hey, you",34.56""") { csv }
        assertResult((classOf[java.lang.Boolean], classOf[String], classOf[java.lang.Float])) {
          val c = sj.read[HasAny](csv)
          (c.a1.getClass, c.a2.getClass, c.a3.getClass)
        }
      }
      it("Accepts value classes as CSV fields") {
        val inst = WithVC(2, VC("blip"), false)
        val csv = sj.render(inst)
        assertResult("""2,blip,false""") { csv }
        assertResult(inst) {
          sj.read[WithVC](csv)
        }
      }
      it("Fails when CSV field count doesn't match case class member count") {
        val csv = """123.45,123,true,64,Z,12.34,Large,12.34,5,5,54cab778-7b9e-4b07-9d37-87b97a011e55"""
        val msg = """Expected value token of type String, not EndObject when reading UUID value.
          |rge,12.34,5,5,54cab778-7b9e-4b07-9d37-87b97a011e55
          |--------------------------------------------------^""".stripMargin
        the[java.lang.IllegalStateException] thrownBy
          sj.read[BasicScala](csv) should have message msg
      }
      it("Fails when types of CSV field don't match ordered case class constructor arguments") {
        val csv = """123.45,"123",true,64,Z,12.34,Large,12.34,5,5,wow,54cab778-7b9e-4b07-9d37-87b97a011e55"""
        val msg = """Expected value token of type Number, not String when reading BigInt value.  (Is your value wrapped in quotes?)
          |123.45,"123",true,64,Z,12.34,Large,12.34,5,5,wow,54cab778-
          |--------^""".stripMargin
        the[java.lang.IllegalStateException] thrownBy
          sj.read[BasicScala](csv) should have message msg
      }
    }
    describe("Fancy String permutations:") {
      it("Handles fancy strings with embedded commas and quotes") {
        val inst = Strings("Hey, you", "This \"life\"", "And now, \"Mike\" will sing.")
        val csv = sj.render(inst)
        assertResult("\"Hey, you\",\"This \"\"life\"\"\",\"And now, \"\"Mike\"\" will sing.\"") { csv }
        assertResult(inst) {
          sj.read[Strings](csv)
        }
      }
      it("Handleds embedded tabs and newlines in strings") {
        val inst = Strings("Hey, you", "This \"life\"", "And\tnow, \"Mike\" will\nsing.")
        val csv = sj.render(inst)
        assertResult("\"Hey, you\",\"This \"\"life\"\"\",\"And\tnow, \"\"Mike\"\" will\nsing.\"") { csv }
        assertResult(inst) {
          sj.read[Strings](csv)
        }
      }
    }
    describe("Options and null:") {
      it("Renders Some()") {
        val inst = Maybe("yes", Some("blink"), true)
        val csv = sj.render(inst)
        assertResult("""yes,blink,true""") { csv }
        assertResult(inst) {
          sj.read[Maybe](csv)
        }
      }
      it("Renders Some() - empty string") {
        val inst = Maybe("yes", Some(""), true)
        val csv = sj.render(inst)
        assertResult("""yes,"",true""") { csv }
        assertResult(inst) {
          sj.read[Maybe](csv)
        }
      }
      it("Renders Some() - fancy string 1") {
        val inst = Maybe("yes", Some("This,test"), true)
        val csv = sj.render(inst)
        assertResult("""yes,"This,test",true""") { csv }
        assertResult(inst) {
          sj.read[Maybe](csv)
        }
      }
      it("Renders Some() - fancy string 2") {
        val inst = Maybe("yes", Some("This \"test\""), true)
        val csv = sj.render(inst)
        assertResult("yes,\"This \"\"test\"\"\",true") { csv }
        assertResult(inst) {
          sj.read[Maybe](csv)
        }
      }
      it("Renders None (empty field)") {
        val inst = Maybe("no", None, false)
        val csv = sj.render(inst)
        assertResult("""no,,false""") { csv }
        assertResult(inst) {
          sj.read[Maybe](csv)
        }
      }
      it("Null objects") {
        val inst: Maybe = null
        val csv = sj.render(inst)
        assertResult("") { csv }
        assertResult(inst) { // null converts to None
          sj.read[Maybe](csv)
        }
      }
      it("Renders null (empty field) - Option") {
        val inst = Maybe("oops", null, false)
        val csv = sj.render(inst)
        assertResult("""oops,,false""") { csv }
        assertResult(Maybe("oops", None, false)) { // null converts to None
          sj.read[Maybe](csv)
        }
      }
      it("Renders null (empty field) - nullable 1") {
        val inst = Strings(null, "two", "three")
        val csv = sj.render(inst)
        assertResult(",two,three") { csv }
        assertResult(inst) { // null converts to empty String
          sj.read[Strings](csv)
        }
      }
      it("Renders null (empty field) - nullable 2") {
        val inst = Strings("", null, "three")
        val csv = sj.render(inst)
        assertResult("\"\",,three") { csv }
        assertResult(inst) { // null converts to empty String
          sj.read[Strings](csv)
        }
      }
      it("Renders null (empty field) - nullable 3") {
        val inst = Strings("", "two", null)
        val csv = sj.render(inst)
        assertResult("\"\",two,") { csv }
        assertResult(inst) { // null converts to empty String
          sj.read[Strings](csv)
        }
      }
    }
    describe("Collections/nested (failures):") {
      it("Fails when given an object having a nested object (not flat)") {
        val inst = Nested("K-9", Thing("Robot dog", 1))
        the[java.lang.UnsupportedOperationException] thrownBy
          sj.render(inst) should have message "Writing a nested object is not supported in CSV format"
      }
      it("Fails when given an object having a nested array (not flat)") {
        val inst = Nested2("K-9", List(Thing("Robot dog", 1)))
        the[java.lang.UnsupportedOperationException] thrownBy
          sj.render(inst) should have message "Writing a nested array is not supported in CSV format"
      }
    }
    describe("ScalaJack creation 'with' modifiers (failure):") {
      it("No withTypeModifier") {
        the[java.lang.UnsupportedOperationException] thrownBy
          ScalaJack(CSVFlavor()).withTypeModifier(null.asInstanceOf[HintModifier]) should have message "Not available for CSV formatting"
      }
      it("No withAdapters") {
        the[java.lang.UnsupportedOperationException] thrownBy
          ScalaJack(CSVFlavor()).withAdapters(null.asInstanceOf[TypeAdapterFactory]) should have message "Not available for CSV formatting"
      }
      it("No withHints") {
        the[java.lang.UnsupportedOperationException] thrownBy
          ScalaJack(CSVFlavor()).withHints(null.asInstanceOf[(Type, String)]) should have message "Not available for CSV formatting"
      }
      it("No withHintModifiers") {
        the[java.lang.UnsupportedOperationException] thrownBy
          ScalaJack(CSVFlavor()).withHintModifiers(null.asInstanceOf[(Type, HintModifier)]) should have message "Not available for CSV formatting"
      }
      it("No withDefaultHint") {
        the[java.lang.UnsupportedOperationException] thrownBy
          ScalaJack(CSVFlavor()).withDefaultHint("") should have message "Not available for CSV formatting"
      }
      it("No parseOrElse") {
        the[java.lang.UnsupportedOperationException] thrownBy
          ScalaJack(CSVFlavor()).parseOrElse(null.asInstanceOf[(Type, Type)]) should have message "Not available for CSV formatting"
      }
      it("No isCanonical") {
        the[java.lang.UnsupportedOperationException] thrownBy
          ScalaJack(CSVFlavor()).isCanonical(false) should have message "Not available for CSV formatting"
      }

    */
    describe("Parsing") {
      it("Tokenizer can grow capacity") {
        val t = new Tokenizer()
        val csv = t.tokenize("foo,bar,blather,stuff,more,and,more".toCharArray, 0, 0, 4)
        println(csv.x)
      }
    }
  }
}
