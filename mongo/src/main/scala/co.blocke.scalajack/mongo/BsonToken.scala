package co.blocke.scalajack
package mongo

import model.ParseToken
import model.TokenType._
import org.bson.BsonValue

object TokenDetail extends Enumeration {
  type TokenDetail = Value

  val Double, Int32, Int64, BigDecimal, Number, Binary, DateTime, ObjectId, NoDetail = Value
}
import TokenDetail._

case class BsonToken(input: BsonValue, tokenType: TokenType, detail: TokenDetail = NoDetail) extends ParseToken[BsonValue] {
  def textValue: String = input.asString.getValue
}
