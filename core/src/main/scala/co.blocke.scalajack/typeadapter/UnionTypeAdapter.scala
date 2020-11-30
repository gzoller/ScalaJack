package co.blocke.scalajack
package typeadapter

import model._
import co.blocke.scala_reflection._
import co.blocke.scala_reflection.info.UnionInfo
import co.blocke.scala_reflection.impl.Clazzes._

import scala.collection.mutable.Builder
import scala.util.{ Failure, Success, Try }

object UnionTypeAdapterFactory extends TypeAdapterFactory:

  def matches(concrete: RType): Boolean = 
    concrete match {
      case _: UnionInfo => true
      case _ => false
    }

  def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[_] =
    val unionInfo = concrete.asInstanceOf[UnionInfo]
    val leftInfo = unionInfo.leftType
    val rightInfo = unionInfo.rightType

    val leftTypeAdapter = taCache.typeAdapterOf(leftInfo)
    val rightTypeAdapter = taCache.typeAdapterOf(rightInfo)

    UnionTypeAdapter(
      concrete,
      leftTypeAdapter,
      rightTypeAdapter)


case class UnionTypeAdapter[L, R](
    info: RType,
    leftTypeAdapter:  TypeAdapter[L],
    rightTypeAdapter: TypeAdapter[R])(implicit taCache: TypeAdapterCache)
  extends TypeAdapter[L | R] {

  override def isStringish: Boolean = leftTypeAdapter.isStringish && rightTypeAdapter.isStringish
  override def maybeStringish: Boolean = !isStringish
  
  def read(parser: Parser): L | R = {
    val savedReader = parser.mark()
    Try(leftTypeAdapter.read(parser)) match {
      case Success(leftValue) => leftValue.asInstanceOf[L]
      case Failure(_) => // Left parse failed... try Right
        parser.revertToMark(savedReader)
        Try(rightTypeAdapter.read(parser)) match {
          case Success(rightValue) => rightValue.asInstanceOf[R]
          case Failure(x) =>
            parser.backspace()
            throw new ScalaJackError( parser.showError(s"Failed to read any values for union type") )
        }
    }
  }

  def write[WIRE](
      t:      L | R,
      writer: Writer[WIRE],
      out:    Builder[WIRE, WIRE]): Unit =
    val trialBuilder = taCache.jackFlavor.getBuilder.asInstanceOf[scala.collection.mutable.Builder[WIRE,WIRE]]
    if Try(leftTypeAdapter.write(t.asInstanceOf[L], writer, trialBuilder)).isFailure then
      rightTypeAdapter.write(t.asInstanceOf[R], writer, out)
    else 
      out += trialBuilder.result
}
