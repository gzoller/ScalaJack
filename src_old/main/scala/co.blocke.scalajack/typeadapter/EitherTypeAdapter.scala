package co.blocke.scalajack
package typeadapter

import model._
import co.blocke.scala_reflection._
import co.blocke.scala_reflection.info.EitherInfo
import co.blocke.scala_reflection.impl.Clazzes._

import scala.collection.mutable.Builder
import scala.util.{ Failure, Success, Try }

object EitherTypeAdapterFactory extends TypeAdapterFactory:

  def matches(concrete: RType): Boolean = 
    concrete match {
      case _: EitherInfo => true
      case _ => false
    }

  def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[_] =
    val eitherInfo = concrete.asInstanceOf[EitherInfo]
    val leftInfo = eitherInfo.leftType
    val rightInfo = eitherInfo.rightType

    if( leftInfo.infoClass <:< rightInfo.infoClass || rightInfo.infoClass <:< leftInfo.infoClass)
      throw new IllegalArgumentException(
        s"Types ${leftInfo.name} and ${rightInfo.name} are not mutually exclusive"
      )
    val leftTypeAdapter = taCache.typeAdapterOf(leftInfo)
    val rightTypeAdapter = taCache.typeAdapterOf(rightInfo)

    EitherTypeAdapter(
      concrete,
      leftTypeAdapter,
      rightTypeAdapter)


case class EitherTypeAdapter[L, R](
    info: RType,
    leftTypeAdapter:  TypeAdapter[L],
    rightTypeAdapter: TypeAdapter[R])
  extends TypeAdapter[Either[L, R]] {

  override def isStringish: Boolean = leftTypeAdapter.isStringish && rightTypeAdapter.isStringish
  override def maybeStringish: Boolean = !isStringish

  def read(parser: Parser): Either[L, R] = {
    val savedReader = parser.mark()
    if (parser.peekForNull)
      null
    else
      Try(rightTypeAdapter.read(parser)) match {
        case Success(rightValue) =>
          Right(rightValue.asInstanceOf[R])
        case Failure(_) => // Right parse failed... try left
          parser.revertToMark(savedReader)
          Try(leftTypeAdapter.read(parser)) match {
            case Success(leftValue) =>
              Left(leftValue.asInstanceOf[L])
            case Failure(x) =>
              parser.backspace()
              throw new ScalaJackError(
                parser.showError(s"Failed to read either side of Either")
              )
          }
      }
  }

  def write[WIRE](
      t:      Either[L, R],
      writer: Writer[WIRE],
      out:    Builder[WIRE, WIRE]): Unit =
    t match {
      case null     => writer.writeNull(out)
      case Left(v)  => leftTypeAdapter.write(v, writer, out)
      case Right(v) => rightTypeAdapter.write(v, writer, out)
    }
}
