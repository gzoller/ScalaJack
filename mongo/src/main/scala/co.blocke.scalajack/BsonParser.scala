package co.blocke.scalajack

import co.blocke.scalajack.json.{Context, TokenType}
import co.blocke.scalajack.json.TokenType.TokenType
import org.bson.{BsonDocument, BsonInt32, BsonString, BsonValue}

import scala.collection.JavaConversions._

case class MongoAddress(city: String, country: String)

case class MongoPerson(name: String, age: Int, address: MongoAddress)

object BsonParser extends App {

  val typeAdapter = Context.StandardContext.typeAdapterOf[MongoPerson]

  val document = new BsonDocument

  document.append("name", new BsonString("Adam"))
  document.append("age", new BsonInt32(32))
  document.append("address", new BsonDocument().append("city", new BsonString("Dallas")).append("country", new BsonString("USA")))

  val parser = new BsonParser
  val reader = parser.parse(document)
  println(reader)

  val person1 = typeAdapter.read(reader)
  println(s"Person #1: $person1")

  val writer = new BsonWriter

  typeAdapter.write(person1, writer)

  println(writer.value)

  val reader2 = parser.parse(writer.value)
  val person2 = typeAdapter.read(reader2)
  println(s"Person #2: $person2")
}

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
      if (value.isArray) {
        val valueAsArray = value.asArray

        appendToken(TokenType.BeginArray, valueAsArray)

        for (value ← valueAsArray) {
          consumeValue(value)
        }

        appendToken(TokenType.EndArray, valueAsArray)
      } else if (value.isBinary) {
        ???
      } else if (value.isBoolean) {
        val valueAsBoolean = value.asBoolean
        if (valueAsBoolean.getValue) {
          appendToken(TokenType.True, valueAsBoolean)
        } else {
          appendToken(TokenType.False, valueAsBoolean)
        }
      } else if (value.isDateTime) {
        ???
      } else if (value.isDBPointer) {
        ???
      } else if (value.isDocument) {
        val valueAsDocument = value.asDocument
        appendToken(TokenType.BeginObject, valueAsDocument)

        for (entry ← valueAsDocument.entrySet) {
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
      } else if (value.isJavaScript) {
        ???
      } else if (value.isJavaScriptWithScope) {
        ???
      } else if (value.isNull) {
        appendToken(TokenType.Null, value)
      } else if (value.isNumber) {
        appendToken(TokenType.Number, value.asNumber)
      } else if (value.isObjectId) {
        ???
      } else if (value.isRegularExpression) {
        ???
      } else if (value.isString) {
        appendString(TokenType.String, value.asString.getValue)
      } else if (value.isSymbol) {
        ???
      } else if (value.isTimestamp) {
        ???
      } else {
        ???
      }
    }

    consumeValue(value)

    new BsonReader(numberOfTokens, tokenTypes, strings, values)
  }

}
