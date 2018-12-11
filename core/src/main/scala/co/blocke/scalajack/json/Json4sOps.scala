package co.blocke.scalajack
package json

import org.json4s._
import model.Ops

object Json4sOps extends Ops[JValue] {
  //  override def getArrayElement(arr: JArray, index: Int): Option[JValue] =
  //    if (index >= 0 && index < arr.values.size) Some(arr(index)) else None

  override def applyArray(values: Seq[JValue]): JValue = new JArray(values.toList)
  override def unapplyArray(ast: JValue): Option[Seq[JValue]] =
    ast match {
      case elements: JArray => Some(elements.children)
      case _                => None
    }

  override def applyBoolean(value: Boolean): JValue = JBool(value)
  override def unapplyBoolean(ast: JValue): Option[Boolean] =
    ast match {
      case JBool(value) => Some(value)
      case _            => None
    }

  override def applyDecimal(value: BigDecimal): JValue = JDecimal(value)
  override def unapplyDecimal(ast: JValue): Option[BigDecimal] =
    ast match {
      case JDecimal(value) => Some(value)
      case _               => None
    }

  override def applyDouble(value: Double): JValue = JDouble(value)
  override def unapplyDouble(ast: JValue): Option[Double] =
    ast match {
      case JDouble(value) => Some(value)
      case _              => None
    }

  override def applyInt(value: BigInt): JValue = JInt(value)
  override def unapplyInt(ast: JValue): Option[BigInt] =
    ast match {
      case JInt(value) => Some(value)
      case _           => None
    }

  override def applyLong(value: Long): JValue = JLong(value)
  override def unapplyLong(ast: JValue): Option[Long] =
    ast match {
      case JLong(value) => Some(value)
      case _            => None
    }

  override def applyNull(): JValue = JNull
  override def unapplyNull(ast: JValue): Boolean =
    ast match {
      case JNull => true
      case _     => false
    }

  override def applyObject(elements: Seq[(String, JValue)]): JValue = new JObject(elements.toList)
  override def unapplyObject(ast: JValue): Option[Seq[(String, JValue)]] =
    ast match {
      case JObject(elements) => Some(elements)
      case _                 => None
    }

  override def applyString(string: String): JValue = JString(string)
  override def unapplyString(ast: JValue): Option[String] =
    ast match {
      case JString(value) => Some(value)
      case _              => None
    }
}
