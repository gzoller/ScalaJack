package co.blocke.scalajack
package json

import org.json4s.JsonAST.{ JArray, JBool, JDecimal, JDouble, JInt, JLong, JNothing, JNull, JObject, JString, JValue }

trait Json4sOpsBase extends AstOps[JValue, String] {

  override type ArrayElements = List[JValue]

  override type ObjectFields = List[(String, JValue)]

  override def foreachArrayElement(elements: List[JValue], f: (Int, JValue) => Unit): Unit = {
    for ((element, index) <- elements.zipWithIndex if element != JNothing) {
      f(index, element)
    }
  }

  override def foreachObjectField(fields: List[(String, JValue)], f: (String, JValue) => Unit): Unit = {
    for ((name, value) <- fields if value != JNothing) {
      f(name, value)
    }
  }

  override def getObjectField(fields: List[(String, JValue)], name: String): Option[JValue] =
    fields.find(_._1 == name).map(_._2).filter(_ != JNothing)

  override def partitionObjectFields(fields: ObjectFields, fieldNames: List[String]): (ObjectFields, ObjectFields) =
    fields.partition(f => fieldNames.contains(f._1))

  override def applyArray(appendAllElements: (JValue => Unit) => Unit): JValue = {
    val elementsBuilder = List.newBuilder[JValue]

    appendAllElements { element =>
      elementsBuilder += element
    }

    JArray(elementsBuilder.result())
  }

  override def unapplyArray(json: JValue): Option[List[JValue]] =
    json match {
      case JArray(elements) => Some(elements)
      case _                => None
    }

  override def applyBoolean(value: Boolean): JValue =
    JBool(value)

  override def unapplyBoolean(json: JValue): Option[Boolean] =
    json match {
      case JBool(value) => Some(value)
      case _            => None
    }

  override def applyDecimal(value: BigDecimal): JValue =
    JDecimal(value)

  override def unapplyDecimal(json: JValue): Option[BigDecimal] =
    json match {
      case JDecimal(value) => Some(value)
      case _               => None
    }

  override def applyDouble(value: Double): JValue =
    JDouble(value)

  override def unapplyDouble(json: JValue): Option[Double] =
    json match {
      case JDouble(value) => Some(value)
      case _              => None
    }

  override def applyInt(value: BigInt): JValue =
    JInt(value)

  override def unapplyInt(json: JValue): Option[BigInt] =
    json match {
      case JInt(value) => Some(value)
      case _           => None
    }

  override def applyLong(value: Long): JValue =
    JLong(value)

  override def unapplyLong(json: JValue): Option[Long] =
    json match {
      case JLong(value) => Some(value)
      case _            => None
    }

  override def applyNull(): JValue =
    JNull

  override def unapplyNull(json: JValue): Boolean =
    json match {
      case JNull => true
      case _     => false
    }

  override def applyObject(appendAllFields: ((String, JValue) => Unit) => Unit): JValue = {
    val fieldsBuilder = List.newBuilder[(String, JValue)]

    appendAllFields { (fieldName, fieldValue) =>
      fieldsBuilder += fieldName -> fieldValue
    }

    JObject(fieldsBuilder.result())
  }

  override def unapplyObject(json: JValue): Option[List[(String, JValue)]] =
    json match {
      case JObject(fields) => Some(fields)
      case _               => None
    }

  override def applyString(string: String): JValue =
    JString(string)

  override def unapplyString(json: JValue): Option[String] =
    json match {
      case JString(value) => Some(value)
      case _              => None
    }

  override def isObject(json: JValue): Boolean = json.isInstanceOf[JObject]
  override def isArray(json: JValue): Boolean = json.isInstanceOf[JArray]
}

object Json4sOps extends Json4sOpsBase {

  val parser: Parser[String] = JsonParser
  val renderer: Renderer[String] = JsonRenderer

}
