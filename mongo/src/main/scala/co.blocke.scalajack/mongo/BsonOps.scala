package co.blocke.scalajack
package mongo

import scala.collection.JavaConverters._
import org.bson.types.Decimal128
import org.bson._
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.MongoClient

object BsonOps extends AstOps[BsonValue, Document] {

  override type ArrayElements = BsonArray
  override type ObjectFields = BsonDocument

  val parser: Parser[Document] = new Parser[Document] {
    def parse[AST](source: Document)(implicit ops: AstOps[AST, Document]): Option[AST] =
      Some(source.toBsonDocument(classOf[BsonDocument], MongoClient.DEFAULT_CODEC_REGISTRY).asInstanceOf[AST])
  }
  val renderer: Renderer[Document] = new Renderer[Document] {
    def renderCompact[AST](ast: AST, sj: ScalaJackLike[_, _])(implicit ops: AstOps[AST, Document]): Document =
      Document(ast.asInstanceOf[BsonValue].asDocument)
  }

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

  override def getObjectField(bson: BsonDocument, name: String): Option[BsonValue] =
    Option(bson.get(name))

  override def partitionObjectFields(fields: ObjectFields, fieldNames: List[String]): (ObjectFields, ObjectFields) = {
    val (one, two) = fields.asInstanceOf[BsonDocument].entrySet.asScala.partition(f => fieldNames.contains(f.getKey))
    def makeBsonDoc(in: Set[java.util.Map.Entry[String, BsonValue]]): BsonDocument =
      new BsonDocument(in.map(s => new BsonElement(s.getKey, s.getValue)).toList.asJava)
    (makeBsonDoc(one.toSet), makeBsonDoc(two.toSet))
  }

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
    new BsonInt32(value.intValue)

  override def unapplyInt(bson: BsonValue): Option[BigInt] =
    if (bson.isInt32) Some(bson.asInt32.getValue) else None

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

  override def isObject(bson: BsonValue): Boolean = bson.isInstanceOf[BsonDocument]
  override def isArray(bson: BsonValue): Boolean = bson.isInstanceOf[BsonArray]

}
