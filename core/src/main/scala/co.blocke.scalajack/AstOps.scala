package co.blocke.scalajack

/**
 * The set of operations required to use [[J]] as a serialization format in ScalaJack.
 *
 * @tparam AST the serialized form (AST)
 *  @tparam S the deserialized form (e.g. String for JSON)
 */
trait AstOps[AST, S] {

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

  val parser: Parser[S]
  val renderer: Renderer[S]

  def parse(src: S): AST = parser.parse[AST](src)(this).getOrElse(throw new IllegalArgumentException("Input is empty"))

  def renderCompact(ast: AST, sj: ScalaJackLike[_, _]): S = renderer.renderCompact[AST](ast, sj)(this)

  def foreachArrayElement(elements: ArrayElements, f: (Int, AST) => Unit): Unit

  def foreachObjectField(fields: ObjectFields, f: (String, AST) => Unit): Unit

  def getObjectField(fields: ObjectFields, name: String): Option[AST]

  // (for SJCapture) Partition out fields we care about (those in the class) from those we just want to capture and hold "raw"
  def partitionObjectFields(fields: ObjectFields, fieldNames: List[String]): (ObjectFields, ObjectFields)

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
  def applyArray(appendAllElements: (AST => Unit) => Unit): AST

  def unapplyArray(json: AST): Option[ArrayElements]

  def applyBoolean(value: Boolean): AST

  def unapplyBoolean(ast: AST): Option[Boolean]

  def applyDecimal(value: BigDecimal): AST

  def unapplyDecimal(ast: AST): Option[BigDecimal]

  def applyDouble(value: Double): AST

  def unapplyDouble(ast: AST): Option[Double]

  def applyInt(value: BigInt): AST

  def unapplyInt(ast: AST): Option[BigInt]

  def applyLong(value: Long): AST

  def unapplyLong(ast: AST): Option[Long]

  def applyNull(): AST

  def unapplyNull(ast: AST): Boolean

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
  // String in tuple here is the field name
  def applyObject(appendAllFields: ((String, AST) => Unit) => Unit): AST

  def unapplyObject(ast: AST): Option[ObjectFields]

  def applyString(string: String): AST

  def unapplyString(ast: AST): Option[String]

  def isObject(ast: AST): Boolean

  def isArray(ast: AST): Boolean

}

object AstOps {
  type Aux[AST, OF, S] = AstOps[AST, S] { type ObjectFields = OF }
}