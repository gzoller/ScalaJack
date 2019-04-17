package co.blocke.scalajack
package typeadapter

import util.Path
import model._

import scala.collection.mutable.Builder
import scala.reflect.runtime.universe.{ NoType, TypeTag, typeOf }
import scala.util.{ Failure, Success, Try }

object UnionTypeAdapterFactory extends TypeAdapterFactory {

  private val unionType = typeOf[_Union].typeSymbol

  override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] = {
    val compType = tt.tpe.dealias
    compType.baseType(unionType) match {
      case NoType =>
        next.typeAdapterOf[T]

      case _ if compType.typeArgs.size == 2 =>
        Union2TypeAdapter(
          context.typeAdapter(compType.typeArgs(0)),
          context.typeAdapter(compType.typeArgs(1))).asInstanceOf[TypeAdapter[T]]

      case _ if compType.typeArgs.size == 3 =>
        Union3TypeAdapter(
          context.typeAdapter(compType.typeArgs(0)),
          context.typeAdapter(compType.typeArgs(1)),
          context.typeAdapter(compType.typeArgs(2))).asInstanceOf[TypeAdapter[T]]

      case _ if compType.typeArgs.size == 4 =>
        Union4TypeAdapter(
          context.typeAdapter(compType.typeArgs(0)),
          context.typeAdapter(compType.typeArgs(1)),
          context.typeAdapter(compType.typeArgs(2)),
          context.typeAdapter(compType.typeArgs(3))).asInstanceOf[TypeAdapter[T]]
    }
  }
}

case class Union2TypeAdapter[A, B](aTa: TypeAdapter[A], bTa: TypeAdapter[B]) extends TypeAdapter[MultiKind2[A, B]] {

  def read[WIRE](path: Path, reader: Reader[WIRE]): MultiKind2[A, B] = {
    val savedReader = reader.copy
    reader.head.tokenType match {
      case TokenType.Null =>
        reader.next
        null
      case _ =>
        Try(aTa.read(path, reader)) match {
          case Success(aValue) =>
            MultiKind2(Some(aValue), None)
          case Failure(_) => // Right parse failed... try left
            reader.syncPositionTo(savedReader)
            Try(bTa.read(path, reader)) match {
              case Success(bValue) =>
                MultiKind2(None, Some(bValue))
              case Failure(x) =>
                throw new ReadMalformedError(reader.showError(path, s"Failed to read any kind of a MultiKind value"))
            }
        }
    }
  }

  def write[WIRE](t: MultiKind2[A, B], writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit = {
    t match {
      case null => writer.writeNull(out)
      case _ => t._unpack match {
        case (v, pos) if pos == 0 =>
          aTa.write(v.asInstanceOf[A], writer, out, isMapKey)
        case (v, pos) if pos == 1 =>
          bTa.write(v.asInstanceOf[B], writer, out, isMapKey)
      }
    }
  }
}

case class Union3TypeAdapter[A, B, C](aTa: TypeAdapter[A], bTa: TypeAdapter[B], cTa: TypeAdapter[C]) extends TypeAdapter[MultiKind3[A, B, C]] {

  def read[WIRE](path: Path, reader: Reader[WIRE]): MultiKind3[A, B, C] = {
    val savedReader = reader.copy
    reader.head.tokenType match {
      case TokenType.Null =>
        reader.next
        null
      case _ =>
        Try(aTa.read(path, reader)) match {
          case Success(aValue) =>
            MultiKind3(Some(aValue), None, None)
          case Failure(_) => // Right parse failed... try left
            reader.syncPositionTo(savedReader)
            Try(bTa.read(path, reader)) match {
              case Success(bValue) =>
                MultiKind3(None, Some(bValue), None)
              case Failure(_) =>
                reader.syncPositionTo(savedReader)
                Try(cTa.read(path, reader)) match {
                  case Success(cValue) =>
                    MultiKind3(None, None, Some(cValue))
                  case Failure(_) =>
                    throw new ReadMalformedError(reader.showError(path, s"Failed to read any kind of a MultiKind value"))
                }
            }
        }
    }
  }

  def write[WIRE](t: MultiKind3[A, B, C], writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit = {
    t match {
      case null => writer.writeNull(out)
      case _ => t._unpack match {
        case (v, pos) if pos == 0 =>
          aTa.write(v.asInstanceOf[A], writer, out, isMapKey)
        case (v, pos) if pos == 1 =>
          bTa.write(v.asInstanceOf[B], writer, out, isMapKey)
        case (v, pos) if pos == 2 =>
          cTa.write(v.asInstanceOf[C], writer, out, isMapKey)
      }
    }
  }
}

case class Union4TypeAdapter[A, B, C, D](aTa: TypeAdapter[A], bTa: TypeAdapter[B], cTa: TypeAdapter[C], dTa: TypeAdapter[D]) extends TypeAdapter[MultiKind4[A, B, C, D]] {

  def read[WIRE](path: Path, reader: Reader[WIRE]): MultiKind4[A, B, C, D] = {
    val savedReader = reader.copy
    reader.head.tokenType match {
      case TokenType.Null =>
        reader.next
        null
      case _ =>
        Try(aTa.read(path, reader)) match {
          case Success(aValue) =>
            MultiKind4(Some(aValue), None, None, None)
          case Failure(_) => // Right parse failed... try left
            reader.syncPositionTo(savedReader)
            Try(bTa.read(path, reader)) match {
              case Success(bValue) =>
                MultiKind4(None, Some(bValue), None, None)
              case Failure(_) =>
                reader.syncPositionTo(savedReader)
                Try(cTa.read(path, reader)) match {
                  case Success(cValue) =>
                    MultiKind4(None, None, Some(cValue), None)
                  case Failure(_) =>
                    reader.syncPositionTo(savedReader)
                    Try(dTa.read(path, reader)) match {
                      case Success(dValue) =>
                        MultiKind4(None, None, None, Some(dValue))
                      case Failure(_) =>
                        throw new ReadMalformedError(reader.showError(path, s"Failed to read any kind of a MultiKind value"))
                    }
                }
            }
        }
    }
  }

  def write[WIRE](t: MultiKind4[A, B, C, D], writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit =
    t match {
      case null => writer.writeNull(out)
      case _ => t._unpack match {
        case (v, pos) if pos == 0 =>
          aTa.write(v.asInstanceOf[A], writer, out, isMapKey)
        case (v, pos) if pos == 1 =>
          bTa.write(v.asInstanceOf[B], writer, out, isMapKey)
        case (v, pos) if pos == 2 =>
          cTa.write(v.asInstanceOf[C], writer, out, isMapKey)
        case (v, pos) if pos == 3 =>
          dTa.write(v.asInstanceOf[D], writer, out, isMapKey)
      }
    }
}