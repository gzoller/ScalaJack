package co.blocke.scalajack
package mongo

import co.blocke.scalajack.TokenType.TokenType
import org.bson.{ BsonDocument, BsonInt32, BsonInt64, BsonString, BsonValue }

import scala.collection.JavaConverters
import scala.collection.JavaConverters._

class BsonParser {

  def parse(value: BsonValue): BsonReader = {

    var numberOfTokens = 0
    val tokenTypes = new Array[TokenType](1024)
    val strings = new Array[String](1024)
    val values = new Array[BsonValue](1024)

    @inline def appendString(tokenType: TokenType, string: String): Unit = {
      val i = numberOfTokens
      numberOfTokens += 1

      tokenTypes(i) = tokenType
      strings(i) = string
    }

    @inline def appendToken(tokenType: TokenType, value: BsonValue): Unit = {
      val i = numberOfTokens
      numberOfTokens += 1

      tokenTypes(i) = tokenType
      values(i) = value
    }

    def consumeValue(value: BsonValue): Unit = {
      if (value == null) {
        // $COVERAGE-OFF$Safety check... It shouldn't be possible for BsonValue to be null
        appendToken(TokenType.Null, value)
        // $COVERAGE-ON$
      } else if (value.isArray) {
        val valueAsArray = value.asArray

        appendToken(TokenType.BeginArray, valueAsArray)

        for (v <- valueAsArray.toArray) {
          consumeValue(v.asInstanceOf[BsonValue])
        }

        appendToken(TokenType.EndArray, valueAsArray)
      } else if (value.isBoolean) {
        val valueAsBoolean = value.asBoolean
        if (valueAsBoolean.getValue) {
          appendToken(TokenType.True, valueAsBoolean)
        } else {
          appendToken(TokenType.False, valueAsBoolean)
        }
      } else if (value.isDateTime) {
        appendToken(TokenType.BeginObject, null)
        appendString(TokenType.String, "$date")
        appendToken(TokenType.Number, new BsonInt64(value.asDateTime.getValue))
        appendToken(TokenType.EndObject, null)
      } else if (value.isDocument) {
        val valueAsDocument = value.asDocument
        appendToken(TokenType.BeginObject, valueAsDocument)

        for (entry <- valueAsDocument.entrySet.asScala) {
          // for (entry <- JavaConverters.asScalaSet(valueAsDocument.entrySet)) {
          appendString(TokenType.String, entry.getKey)
          consumeValue(entry.getValue)
        }

        appendToken(TokenType.EndObject, valueAsDocument)
      } else if (value.isDouble) {
        appendToken(TokenType.Number, value.asDouble)
      } else if (value.isInt32) {
        appendToken(TokenType.Number, value.asInt32)
      } else if (value.isInt64) {
        appendToken(TokenType.Number, value.asInt64)
      } else if (value.isNull) {
        appendToken(TokenType.Null, value)
      } else if (value.isNumber) {
        appendToken(TokenType.Number, value.asNumber)
      } else if (value.isObjectId) {
        appendToken(TokenType.BeginObject, null)
        appendString(TokenType.String, "$oid")
        appendString(TokenType.String, value.asObjectId.getValue.toHexString)
        appendToken(TokenType.EndObject, null)
      } else if (value.isString) {
        appendString(TokenType.String, value.asString.getValue)
      } else {
        throw new IllegalArgumentException(s"Type for value $value is either deprecated by Mongo, or unsupported by ScalaJack as unsafe (e.g. Javascript)")
      }
    }

    consumeValue(value)

    new BsonReader(numberOfTokens, tokenTypes, strings, values)
  }

}
