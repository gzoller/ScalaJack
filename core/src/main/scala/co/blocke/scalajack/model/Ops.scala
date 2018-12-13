package co.blocke.scalajack
package model

trait Ops[AST] {

  self =>

  def applyArray(value: Seq[AST]): AST
  def unapplyArray(ast: AST): Option[Seq[AST]]

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

  def applyObject(elements: Seq[(String, AST)]): AST
  def unapplyObject(ast: AST): Option[Seq[(String, AST)]]

  def applyString(value: String): AST
  def unapplyString(ast: AST): Option[String]

  /*
  def getArrayElement(arr: ArrayType, index: Int): Option[AST]

  //------- Operations you get "for free", i.e. no need to implement these for a new AST

  def partitionObject(obj: AST, fn: (String, AST) => Boolean): (AST, AST) = obj match {
    case ASTObject(elements) =>
      val m1 = scala.collection.mutable.ListBuffer.empty[(String, AST)]
      val m2 = scala.collection.mutable.ListBuffer.empty[(String, AST)]
      elements.map(e => if (fn(e._1, e._2)) m1 += e else m2 += e)
      (ASTObject(m1), ASTObject(m2))
    case _ => throw new IllegalArgumentException("partitionObject() requires ASTObject as input")
  }

  def mapArrayElements[A](ir: AST, f: (AST, Int) => A): List[A] = ir match {
    case ASTArray(elements) => elements.zipWithIndex.map(a => f(a._1, a._2)).toList
    case _                 => throw new IllegalArgumentException("mapArrayElements() requires ASTArray as input")
  }

  def mapObjectFields[A](ir: AST, f: (String, AST) => A): List[A] = ir match {
    case ASTObject(fields) => fields.map { case (k, v) => f(k, v) }.toList
    case _                => throw new IllegalArgumentException("mapObjectFields() requires ASTObject as input")
  }

  def become[ASTB](source: AST)(implicit targetOps: OpsBase[ASTB]): ASTB =
    if (this == targetOps) {
      source.asInstanceOf[ASTB]
    } else {
      source match {
        case ASTArray(elements)       => ASTArray[ASTB](elements.map(e => become(e)(targetOps)))
        case ASTBoolean(booleanValue) => ASTBoolean[ASTB](booleanValue)
        case ASTDecimal(bigDecimal)   => ASTDecimal[ASTB](bigDecimal)
        case ASTDouble(doubleValue)   => ASTDouble[ASTB](doubleValue)
        case ASTInt(bigInt)           => ASTInt[ASTB](bigInt)
        case ASTLong(longValue)       => ASTLong[ASTB](longValue)
        case ASTNull()                => ASTNull[ASTB]()
        case ASTObject(elements)      => ASTObject[ASTB](elements.map { case (k, v) => (k, become(v)(targetOps)) })
        case ASTString(string)        => ASTString[ASTB](string)
      }
    }
    */
}

/*
object Ops {
  type Aux[AST, WASTE, OBJ] = Ops[AST, WASTE] { type ObjectType = OBJ }
}

// The class, objects, and traits below are used exclusively to support SJCapture capability
//-----------------------------------
trait ASTAndOps {
  type ObjectType
  type ASTType
  type WireType
  val capturedFields: ObjectType
  implicit val ops: Ops[ASTType, WireType]
}

object ASTAndOps {
  def apply[AST, WASTE, OBJ](captured: OBJ)(implicit opsx: Ops.Aux[AST, WASTE, OBJ]): ASTAndOps =
    new ASTAndOps {
      override type ObjectType = OBJ
      override type ASTType = AST
      override type WireType = WASTE
      override val capturedFields: ObjectType = captured
      override implicit val ops: Ops[ASTType, WireType] = opsx
    }
}
*/ 