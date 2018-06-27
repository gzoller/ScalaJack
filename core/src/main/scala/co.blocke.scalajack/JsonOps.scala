package co.blocke.scalajack

/**
 * The set of operations required to use [[J]] as a serialization format in ScalaJack.
 *
 * @tparam J the serialized form
 */
trait JsonOps[J] {

  /**
   * A representation of an array's elements suitable to this serialized form.
   */
  type ArrayElements

  /**
   * A representation of an object's fields suitable to this serialized form. For example, Json4s defines a JSON object
   * containing a [[List[(String, JValue)]]. Therefore, [[Json4sOps#ObjectFields]] is defined as [[List[(String, JValue)]].
   * Other libraries may define a JSON object as a [[Map]] instead of a [[List]].
   */
  type ObjectFields

  def parse(string: String): J = JsonParser.parse[J](string)(this).getOrElse(throw new IllegalArgumentException("JSON string is empty"))

  def renderCompact(json: J): String = JsonRenderer.renderCompact[J](json)(this)

  def foreachArrayElement(elements: ArrayElements, f: (Int, J) => Unit): Unit

  def foreachObjectField(fields: ObjectFields, f: (String, J) => Unit): Unit

  def getObjectField(fields: ObjectFields, name: String): Option[J]

  /**
   *
   * {{{
   *   val json: J = ops applyArray { appendElement =>
   *     appendElement(ops.applyString("first"))
   *     appendElement(ops.applyString("second"))
   *     appendElement(ops.applyString("third"))
   *   }
   * }}}
   *
   * @param appendAllElements A function that will append all the array's element by repeatedly calling the [[J => Unit]] function that *it* is handed.
   * @return
   */
  def applyArray(appendAllElements: (J => Unit) => Unit): J

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

  def unapplyNull(json: J): Boolean

  /**
   *
   * {{{
   *   val json: J = ops applyObject { appendField =>
   *     appendField("name", ops.applyString("Adam"))
   *     appendField("favoriteColor", ops.applyString("Blue"))
   *   }
   * }}}
   *
   * @param appendAllFields
   * @return
   */
  def applyObject(appendAllFields: ((String, J) => Unit) => Unit): J

  def unapplyObject(json: J): Option[ObjectFields]

  def applyString(string: String): J

  def unapplyString(json: J): Option[String]

}
