package co.blocke.scalajack
package mongo

import org.bson.types.Decimal128
import org.bson.{ BsonArray, BsonBoolean, BsonDecimal128, BsonDocument, BsonDouble, BsonElement, BsonInt64, BsonNull, BsonString, BsonValue }

object BsonOps extends JsonOps[BsonValue] {

  override type ArrayElements = BsonArray

  override type ObjectFields = BsonDocument

  override def foreachArrayElement(bson: BsonArray, f: (Int, BsonValue) => Unit): Unit = {
    val iterator = bson.iterator
    var i = 0
    while (iterator.hasNext) {
      val element = iterator.next()
      f(i, element)
      i += 1
    }
  }

  override def foreachObjectField(bson: BsonDocument, f: (String, BsonValue) => Unit): Unit = {
    val iterator = bson.entrySet.iterator
    while (iterator.hasNext) {
      val entry = iterator.next()
      f(entry.getKey, entry.getValue)
    }
  }

  override def findObjectField(bson: BsonDocument, name: String): Option[BsonValue] =
    Option(bson.get(name))

  override def applyArray(appendAllElements: (BsonValue => Unit) => Unit): BsonValue = {
    val values = new java.util.ArrayList[BsonValue]

    appendAllElements { element =>
      values.add(element)
    }

    new BsonArray(values)
  }

  override def unapplyArray(bson: BsonValue): Option[BsonArray] =
    if (bson.isArray) Some(bson.asArray) else None

  override def applyBoolean(value: Boolean): BsonValue =
    BsonBoolean.valueOf(value)

  override def unapplyBoolean(bson: BsonValue): Option[Boolean] =
    if (bson.isBoolean) Some(bson.asBoolean.getValue) else None

  override def applyDecimal(value: BigDecimal): BsonValue =
    new BsonDecimal128(new Decimal128(value.bigDecimal))

  override def unapplyDecimal(bson: BsonValue): Option[BigDecimal] =
    if (bson.isDecimal128) Some(bson.asDecimal128.getValue.bigDecimalValue) else None

  override def applyDouble(value: Double): BsonValue =
    new BsonDouble(value)

  override def unapplyDouble(bson: BsonValue): Option[Double] =
    if (bson.isDouble) Some(bson.asDouble.getValue) else None

  override def applyInt(value: BigInt): BsonValue =
    new BsonDecimal128(new Decimal128(new java.math.BigDecimal(value.bigInteger)))

  override def unapplyInt(bson: BsonValue): Option[BigInt] =
    None

  override def applyLong(value: Long): BsonValue =
    new BsonInt64(value)

  override def unapplyLong(bson: BsonValue): Option[Long] =
    if (bson.isInt64) Some(bson.asInt64.getValue) else None

  override def applyNull(): BsonValue =
    BsonNull.VALUE

  override def unapplyNull(bson: BsonValue): Boolean =
    bson.isNull

  override def applyObject(appendAllFields: ((String, BsonValue) => Unit) => Unit): BsonValue = {
    val elements = new java.util.ArrayList[BsonElement]

    appendAllFields { (fieldName, fieldValue) =>
      elements.add(new BsonElement(fieldName, fieldValue))
    }

    new BsonDocument(elements)
  }

  override def unapplyObject(bson: BsonValue): Option[BsonDocument] =
    if (bson.isDocument) Some(bson.asDocument) else None

  override def applyString(string: String): BsonValue =
    new BsonString(string)

  override def unapplyString(bson: BsonValue): Option[String] =
    if (bson.isString) Some(bson.asString.getValue) else None

}
