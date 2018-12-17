package co.blocke.scalajack
package json

import play.api.libs.json._
import model.Ops

object PlayJsonOps extends Ops[JsValue] {
  //  override def getArrayElement(arr: JArray, index: Int): Option[JValue] =
  //    if (index >= 0 && index < arr.values.size) Some(arr(index)) else None

  override def applyArray(values: Seq[JsValue]): JsValue = new JsArray(values.toIndexedSeq)
  override def unapplyArray(ast: JsValue): Option[Seq[JsValue]] =
    ast match {
      case JsArray(elements) => Some(elements)
      case _                 => None
    }

  override def applyBoolean(value: Boolean): JsValue = JsBoolean(value)
  override def unapplyBoolean(ast: JsValue): Option[Boolean] =
    ast match {
      case JsBoolean(value) => Some(value)
      case JsTrue           => Some(true)
      case JsFalse          => Some(false)
      case _                => None
    }

  override def applyDecimal(value: BigDecimal): JsValue = JsNumber(value)
  override def unapplyDecimal(ast: JsValue): Option[BigDecimal] =
    ast match {
      case JsNumber(value) => Some(value)
      case _               => None
    }

  override def applyDouble(value: Double): JsValue = JsNumber(value)
  override def unapplyDouble(ast: JsValue): Option[Double] =
    ast match {
      case JsNumber(value) => Some(value.doubleValue())
      case _               => None
    }

  override def applyInt(value: BigInt): JsValue = JsNumber(BigDecimal(value))
  override def unapplyInt(ast: JsValue): Option[BigInt] =
    ast match {
      case JsNumber(value) => Some(value.toBigInt)
      case _               => None
    }

  override def applyLong(value: Long): JsValue = JsNumber(value)
  override def unapplyLong(ast: JsValue): Option[Long] =
    ast match {
      case JsNumber(value) => Some(value.toLongExact)
      case _               => None
    }

  override def applyNull(): JsValue = JsNull
  override def unapplyNull(ast: JsValue): Boolean =
    ast match {
      case JsNull => true
      case _      => false
    }

  override def applyObject(elements: Seq[(String, JsValue)]): JsValue = new JsObject(elements.toMap)
  override def unapplyObject(ast: JsValue): Option[Seq[(String, JsValue)]] =
    ast match {
      case JsObject(elements) => Some(elements.toSeq)
      case _                  => None
    }

  override def applyString(string: String): JsValue = JsString(string)
  override def unapplyString(ast: JsValue): Option[String] =
    ast match {
      case JsString(value) => Some(value)
      case _               => None
    }
}
