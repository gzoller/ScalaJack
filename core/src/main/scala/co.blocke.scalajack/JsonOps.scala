package co.blocke.scalajack

trait JsonOps[J] {

  type ArrayElements

  type ObjectFields

  def foreachArrayElement(elements: ArrayElements, f: (Int, J) => Unit): Unit

  def foreachObjectField(fields: ObjectFields, f: (String, J) => Unit): Unit

  def applyArray(build: J => Unit): J

  def unapplyArray(json: J): Option[ArrayElements]

  def applyBoolean(value: Boolean): J

  def unapplyBoolean(json: J): Option[Boolean]

  def applyDecimal(value: BigDecimal): J

  def unapplyDecimal(json: J): Option[BigDecimal]

  def applyDouble(value: Double): J

  def unapplyDouble(json: J): Option[Double]

  def applyInt(value: BigInt): J

  def unapplyInt(json: J): Option[BigInt]

  def applyLong(value: Long): J

  def unapplyLong(json: J): Option[Long]

  def applyNull(): J

  def unapplyNull(): Boolean

  def applyObject(build: (String, J) => Unit): J

  def unapplyObject(json: J): Option[ObjectFields]

  def applyString(string: String): J

  def unapplyString(json: J): Option[String]

}
