package co.blocke.scalajack

import org.json4s.JsonAST.{ JArray, JBool, JDecimal, JDouble, JInt, JLong, JNull, JObject, JString, JValue }

object Json4sOps extends JsonOps[JValue] {

  override type ArrayElements = List[JValue]

  override type ObjectFields = List[(String, JValue)]

  override def foreachArrayElement(elements: List[JValue], f: (Int, JValue) => Unit): Unit = {
    for ((element, index) <- elements.zipWithIndex) {
      f(index, element)
    }
  }

  override def foreachObjectField(fields: List[(String, JValue)], f: (String, JValue) => Unit): Unit = {
    for ((name, value) <- fields) {
      f(name, value)
    }
  }

  override def applyArray(build: (JValue => Unit) => Unit): JValue = {
    val elementsBuilder = List.newBuilder[JValue]

    def appendElement(element: JValue): Unit = {
      elementsBuilder += element
    }

    build(appendElement)

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

  override def applyObject(build: ((String, JValue) => Unit) => Unit): JValue = {
    val fieldsBuilder = List.newBuilder[(String, JValue)]

    def appendField(name: String, value: JValue): Unit = {
      fieldsBuilder += name -> value
    }

    build(appendField)

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

}
