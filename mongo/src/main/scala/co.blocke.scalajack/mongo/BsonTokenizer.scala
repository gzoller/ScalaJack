package co.blocke.scalajack
package mongo

import java.util.ArrayList

import model._
import model.TokenType._
import org.bson._
import scala.collection.JavaConverters._

case class BsonTokenizer() extends Tokenizer[BsonValue] {

  def tokenize(source: BsonValue): ArrayList[ParseToken[BsonValue]] = {
    val tokenspace = new ArrayList[ParseToken[BsonValue]]()

    @inline def addString(string: BsonString): Unit = tokenspace.add(BsonToken(string, String))

    def consumeValue(value: BsonValue): Unit =
      value match {
        case v if v.isArray =>
          tokenspace.add(BsonToken(new BsonString("array"), TokenType.BeginArray))
          for (v <- v.asArray().toArray) {
            consumeValue(v.asInstanceOf[BsonValue])
          }
          tokenspace.add(BsonToken(new BsonString("array"), TokenType.EndArray))

        //        case v if v.isBinary =>
        case v if v.isBoolean =>
          tokenspace.add(BsonToken(v, TokenType.Boolean))
        case v if v.isDateTime =>
          tokenspace.add(BsonToken(new BsonInt64(v.asDateTime.getValue), TokenType.Number, TokenDetail.DateTime))
        case v if v.isDecimal128 =>
          tokenspace.add(BsonToken(v, TokenType.Number, TokenDetail.BigDecimal))

        case v if v.isDocument() =>
          tokenspace.add(BsonToken(new BsonString("object"), TokenType.BeginObject))
          for (entry <- v.asDocument.entrySet.asScala) {
            addString(new BsonString(entry.getKey))
            consumeValue(entry.getValue)
          }
          tokenspace.add(BsonToken(new BsonString("object"), TokenType.EndObject))

        case v if v.isDouble =>
          tokenspace.add(BsonToken(v, TokenType.Number, TokenDetail.Double))
        case v if v.isInt32 =>
          tokenspace.add(BsonToken(v, TokenType.Number, TokenDetail.Int32))
        case v if v.isInt64 =>
          tokenspace.add(BsonToken(v, TokenType.Number, TokenDetail.Int64))
        case v if v.isNull =>
          tokenspace.add(BsonToken(null, TokenType.Null))
        case v if v.isNumber =>
          tokenspace.add(BsonToken(v, TokenType.Number, TokenDetail.Number))
        case v if v.isObjectId =>
          tokenspace.add(BsonToken(v, TokenType.String, TokenDetail.ObjectId))
        case v if v.isString =>
          addString(v.asString)
        //        case v if v.isTimestamp =>
        case v =>
          throw new SJError(s"BSON type ${v.getClass.getName} is not currently supported in ScalaJack.")
      }

    consumeValue(source)

    tokenspace.add(BsonToken(null, End))
    tokenspace
  }
}
