package co.blocke.scalajack
package typeadapter

import model._

import scala.collection.mutable
import scala.reflect.runtime.universe.{ NoType, TypeTag, typeOf }
import scala.util.{ Failure, Success, Try }

object UnionTypeAdapterFactory extends TypeAdapterFactory {

  private val unionType = typeOf[_Union].typeSymbol

  override def typeAdapterOf[T](
      next: TypeAdapterFactory
  )(implicit taCache: TypeAdapterCache, tt: TypeTag[T]): TypeAdapter[T] = {
    val compType = tt.tpe.dealias
    compType.baseType(unionType) match {
      case NoType =>
        next.typeAdapterOf[T]

      case _ if compType.typeArgs.size == 2 =>
        Union2TypeAdapter(
          taCache.typeAdapter(compType.typeArgs.head),
          taCache.typeAdapter(compType.typeArgs(1))
        ).asInstanceOf[TypeAdapter[T]]

      case _ if compType.typeArgs.size == 3 =>
        Union3TypeAdapter(
          taCache.typeAdapter(compType.typeArgs.head),
          taCache.typeAdapter(compType.typeArgs(1)),
          taCache.typeAdapter(compType.typeArgs(2))
        ).asInstanceOf[TypeAdapter[T]]

      case _ if compType.typeArgs.size == 4 =>
        Union4TypeAdapter(
          taCache.typeAdapter(compType.typeArgs.head),
          taCache.typeAdapter(compType.typeArgs(1)),
          taCache.typeAdapter(compType.typeArgs(2)),
          taCache.typeAdapter(compType.typeArgs(3))
        ).asInstanceOf[TypeAdapter[T]]
    }
  }
}

case class Union2TypeAdapter[A, B](aTa: TypeAdapter[A], bTa: TypeAdapter[B])
  extends TypeAdapter[Union2[A, B]] {

  def read(parser: Parser): Union2[A, B] =
    if (parser.peekForNull)
      null.asInstanceOf[Union2[A, B]]
    else {
      val savedReader = parser.mark()
      Try(aTa.read(parser)) match {
        case Success(aValue) =>
          Union2(Some(aValue), None)
        case Failure(_) => // Right parse failed... try left
          parser.revertToMark(savedReader)
          Try(bTa.read(parser)) match {
            case Success(bValue) =>
              Union2(None, Some(bValue))
            case Failure(_) =>
              throw new ScalaJackError(
                parser.showError(s"Failed to read any kind of a Union value")
              )
          }
      }
    }

  def write[WIRE](
      t:      Union2[A, B],
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit = {
    t match {
      case null =>
        writer.writeNull(out)
      case _ =>
        t._unpack match {
          case (v, pos) if pos == 0 =>
            aTa.write(v.asInstanceOf[A], writer, out)
          case (v, pos) if pos == 1 =>
            bTa.write(v.asInstanceOf[B], writer, out)
        }
    }
  }
}

case class Union3TypeAdapter[A, B, C](
    aTa: TypeAdapter[A],
    bTa: TypeAdapter[B],
    cTa: TypeAdapter[C])
  extends TypeAdapter[Union3[A, B, C]] {

  def read(parser: Parser): Union3[A, B, C] =
    if (parser.peekForNull)
      null.asInstanceOf[Union3[A, B, C]]
    else {
      val savedReader = parser.mark()
      Try(aTa.read(parser)) match {
        case Success(aValue) =>
          Union3(Some(aValue), None, None)
        case Failure(_) => // Right parse failed... try left
          parser.revertToMark(savedReader)
          Try(bTa.read(parser)) match {
            case Success(bValue) =>
              Union3(None, Some(bValue), None)
            case Failure(_) =>
              parser.revertToMark(savedReader)
              Try(cTa.read(parser)) match {
                case Success(cValue) =>
                  Union3(None, None, Some(cValue))
                case Failure(_) =>
                  throw new ScalaJackError(
                    parser
                      .showError(s"Failed to read any kind of a Union value")
                  )
              }
          }
      }
    }

  def write[WIRE](
      t:      Union3[A, B, C],
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit = {
    t match {
      case null =>
        writer.writeNull(out)
      case _ =>
        t._unpack match {
          case (v, pos) if pos == 0 =>
            aTa.write(v.asInstanceOf[A], writer, out)
          case (v, pos) if pos == 1 =>
            bTa.write(v.asInstanceOf[B], writer, out)
          case (v, pos) if pos == 2 =>
            cTa.write(v.asInstanceOf[C], writer, out)
        }
    }
  }
}

case class Union4TypeAdapter[A, B, C, D](
    aTa: TypeAdapter[A],
    bTa: TypeAdapter[B],
    cTa: TypeAdapter[C],
    dTa: TypeAdapter[D])
  extends TypeAdapter[Union4[A, B, C, D]] {

  def read(parser: Parser): Union4[A, B, C, D] =
    if (parser.peekForNull)
      null.asInstanceOf[Union4[A, B, C, D]]
    else {
      val savedReader = parser.mark()
      Try(aTa.read(parser)) match {
        case Success(aValue) =>
          Union4(Some(aValue), None, None, None)
        case Failure(_) => // Right parse failed... try left
          parser.revertToMark(savedReader)
          Try(bTa.read(parser)) match {
            case Success(bValue) =>
              Union4(None, Some(bValue), None, None)
            case Failure(_) =>
              parser.revertToMark(savedReader)
              Try(cTa.read(parser)) match {
                case Success(cValue) =>
                  Union4(None, None, Some(cValue), None)
                case Failure(_) =>
                  parser.revertToMark(savedReader)
                  Try(dTa.read(parser)) match {
                    case Success(dValue) =>
                      Union4(None, None, None, Some(dValue))
                    case Failure(_) =>
                      throw new ScalaJackError(
                        parser.showError(
                          s"Failed to read any kind of a Union value"
                        )
                      )
                  }
              }
          }
      }
    }

  def write[WIRE](
      t:      Union4[A, B, C, D],
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit =
    t match {
      case null =>
        writer.writeNull(out)
      case _ =>
        t._unpack match {
          case (v, pos) if pos == 0 =>
            aTa.write(v.asInstanceOf[A], writer, out)
          case (v, pos) if pos == 1 =>
            bTa.write(v.asInstanceOf[B], writer, out)
          case (v, pos) if pos == 2 =>
            cTa.write(v.asInstanceOf[C], writer, out)
          case (v, pos) if pos == 3 =>
            dTa.write(v.asInstanceOf[D], writer, out)
        }
    }
}
