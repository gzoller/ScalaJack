package co.blocke.scalajackx

trait Serializer extends AstAdapter with WireSerializer

trait AstOps[AST] {

  self =>

  implicit val selfOps = self

  type ObjectType
  type ArrayType

  def applyArray(value: Seq[AST]): AST
  def unapplyArray(ir: AST): Option[Seq[AST]]

  def applyBoolean(value: Boolean): AST
  def unapplyBoolean(ir: AST): Option[Boolean]

  /*
  def applyDecimal(value: BigDecimal): AST
  def unapplyDecimal(ir: AST): Option[BigDecimal]

  def applyDouble(value: Double): AST
  def unapplyDouble(ir: AST): Option[Double]

  def applyInt(value: BigInt): AST
  def unapplyInt(ir: AST): Option[BigInt]

  def applyLong(value: Long): AST
  def unapplyLong(ir: AST): Option[Long]

  def applyNull(): AST
  def unapplyNull(ir: AST): Boolean

  def applyObject(elements: Seq[(String, AST)]): AST
  def unapplyObject(ir: AST): Option[Seq[(String, AST)]]

  def applyString(value: String): AST
  def unapplyString(ir: AST): Option[String]

  def getArrayElement(arr: ArrayType, index: Int): Option[AST]
*/
}