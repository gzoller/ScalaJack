package co.blocke.scalajack

import co.blocke.scalajack.JsonValueType.JsonValueType
import org.scalatest.FunSpec
import org.scalatest.Matchers._

object JsonValueType extends Enumeration {
  type JsonValueType = Value
  val JsonArrayType, JsonBooleanType, JsonDecimalType, JsonDoubleType, JsonIntType, JsonLongType, JsonNullType, JsonObjectType, JsonStringType = Value
}

trait JsonOpsSpec[J] extends FunSpec {

  val ops: JsonOps[J]

  def sniffJsonValueTypes(jsonValue: J): Set[JsonValueType] = {
    val builder = Set.newBuilder[JsonValueType]
    if (ops.unapplyArray(jsonValue).isDefined) builder += JsonValueType.JsonArrayType
    if (ops.unapplyBoolean(jsonValue).isDefined) builder += JsonValueType.JsonBooleanType
    if (ops.unapplyDecimal(jsonValue).isDefined) builder += JsonValueType.JsonDecimalType
    if (ops.unapplyDouble(jsonValue).isDefined) builder += JsonValueType.JsonDoubleType
    if (ops.unapplyInt(jsonValue).isDefined) builder += JsonValueType.JsonIntType
    if (ops.unapplyLong(jsonValue).isDefined) builder += JsonValueType.JsonLongType
    if (ops.unapplyNull(jsonValue)) builder += JsonValueType.JsonNullType
    if (ops.unapplyObject(jsonValue).isDefined) builder += JsonValueType.JsonObjectType
    if (ops.unapplyString(jsonValue).isDefined) builder += JsonValueType.JsonStringType
    builder.result()
  }

  def sniffJsonValueType(jsonValue: J): JsonValueType = {
    val jsonValueTypes = sniffJsonValueTypes(jsonValue)
    if (jsonValueTypes.isEmpty) {
      throw new IllegalArgumentException(s"Cannot determine JSON value type of $jsonValue")
    } else if (jsonValueTypes.size > 1) {
      throw new IllegalArgumentException(s"$jsonValue is actually ${jsonValueTypes.size} types: ${jsonValueTypes.mkString(", ")}")
    } else {
      jsonValueTypes.head
    }
  }

  describe("JsonOps") {
    it("it should create a JSON array") {
      val expectedElements: List[J] = (for (i <- 1 to 10) yield ops.applyString("element" + i)).toList

      val json = ops.applyArray { appendElement =>
        expectedElements.foreach(appendElement)
      }
      sniffJsonValueTypes(json) should be(Set(JsonValueType.JsonArrayType))

      val actualElementsBuilder = List.newBuilder[J]

      ops.foreachArrayElement(ops.unapplyArray(json).get.asInstanceOf[ops.ArrayElements], { (index, element) =>
        actualElementsBuilder += element
      })

      val actualElements: List[J] = actualElementsBuilder.result()
      actualElements should be(expectedElements)
    }

    it("should create a JSON boolean (false)") {
      val json = ops.applyBoolean(false)
      ops.unapplyBoolean(json) should be(Some(false))
      sniffJsonValueTypes(json) should be(Set(JsonValueType.JsonBooleanType))
    }

    it("should create a JSON boolean (true)") {
      val json = ops.applyBoolean(true)
      ops.unapplyBoolean(json) should be(Some(true))
      sniffJsonValueTypes(json) should be(Set(JsonValueType.JsonBooleanType))
    }

    describe("numbers") {
      it("should create a JSON decimal") {
        val json = ops.applyDecimal(BigDecimal("12.34"))
        sniffJsonValueTypes(json) should be(Set(JsonValueType.JsonDecimalType))
        ops.unapplyDecimal(json) should be(Some(BigDecimal("12.34")))
      }

      it("should create a JSON double") {
        val doubleValue: Double = 12.34
        val json = ops.applyDouble(doubleValue)
        sniffJsonValueType(json) match {
          case JsonValueType.JsonDecimalType =>
            ops.unapplyDecimal(json) should be(Some(BigDecimal(doubleValue)))

          case JsonValueType.JsonDoubleType =>
            ops.unapplyDouble(json) should be(Some(doubleValue))
        }
      }

      it("should create a JSON int") {
        val bigIntValue = BigInt(1234)
        val json = ops.applyInt(bigIntValue)
        sniffJsonValueType(json) match {
          case JsonValueType.JsonDecimalType =>
            ops.unapplyDecimal(json) should be(Some(BigDecimal(bigIntValue)))

          case JsonValueType.JsonDoubleType =>
            ops.unapplyDouble(json) should be(Some(1234.0))

          case JsonValueType.JsonIntType =>
            ops.unapplyInt(json) should be(Some(bigIntValue))

          case JsonValueType.JsonLongType =>
            ops.unapplyLong(json) should be(Some(1234L))
        }
      }

      it("should create a JSON long") {
        val longValue = 1234L
        val json = ops.applyLong(longValue)
        sniffJsonValueType(json) match {
          case JsonValueType.JsonDecimalType =>
            ops.unapplyDecimal(json) should be(Some(BigDecimal(longValue)))

          case JsonValueType.JsonDoubleType =>
            ops.unapplyDouble(json) should be(Some(longValue.toDouble))

          case JsonValueType.JsonIntType =>
            ops.unapplyInt(json) should be(Some(BigInt(longValue)))

          case JsonValueType.JsonLongType =>
            ops.unapplyLong(json) should be(Some(longValue))
        }
      }
    }

    it("should create a JSON null") {
      val json = ops.applyNull()
      ops.unapplyNull(json) should be(true)
      sniffJsonValueTypes(json) should be(Set(JsonValueType.JsonNullType))
    }

    it("should create a JSON object") {
      val expectedFields: List[(String, J)] = (for (i <- 1 to 10) yield ("key" + i, ops.applyString("value" + i))).toList

      val json = ops.applyObject { appendField =>
        for ((fieldName, fieldValue) <- expectedFields) {
          appendField(fieldName, fieldValue)
        }
      }

      sniffJsonValueTypes(json) should be(Set(JsonValueType.JsonObjectType))

      val actualFieldsBuilder = List.newBuilder[(String, J)]

      ops.foreachObjectField(ops.unapplyObject(json).get.asInstanceOf[ops.ObjectFields], { (fieldName, fieldValue) =>
        actualFieldsBuilder += fieldName -> fieldValue
      })

      val actualFields: List[(String, J)] = actualFieldsBuilder.result()

      actualFields should be(expectedFields)
    }

    it("should create a JSON string") {
      val json = ops.applyString("Banana")
      ops.unapplyString(json) should be(Some("Banana"))
      sniffJsonValueTypes(json) should be(Set(JsonValueType.JsonStringType))
    }
  }

}
