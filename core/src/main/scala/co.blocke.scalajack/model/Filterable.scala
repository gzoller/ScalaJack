package co.blocke.scalajack
package model

import util.Path
import scala.util.{ Try, Success }

trait Filterable[WIRE] {

  this: JackFlavor[WIRE] =>

  case class Filter[T](filter: PartialFunction[Reader[WIRE], Option[T]]) {
    def unapply(reader: model.Reader[WIRE]): Option[T] = {
      reader.reset
      filter.apply(reader)
    }
  }

  // Like JackFlavor.read, except here we already have parsed input, hence a Reader is available
  private def _read[T](reader: Reader[WIRE])(implicit tt: TypeTag[T]): T = {
    val v = context.typeAdapter(tt.tpe).read(Path.Root, reader).asInstanceOf[T]
    if (reader.hasNext && reader.head.tokenType != TokenType.End)
      throw new ReadInvalidError(reader.showError(Path.Root, "Extra input after read."))
    v
  }

  def filter[T](hintLabel: String = "")(implicit tt: TypeTag[T]): Filter[T] = {
    val partialFunc: PartialFunction[Reader[WIRE], Option[T]] = {

      case reader: Reader[WIRE] if hintLabel.length == 0 =>
        Try(_read(reader)(tt)).toOption

      case reader: Reader[WIRE] if (hintLabel.length > 0) && {
        val result = reader.scanForHint(hintLabel) match {
          case Some(hintValue) =>
            reader.jackFlavor.typeValueModifier match {
              case Some(fn) => // apply type value modifier if there is one (may explode!)
                try {
                  val foundType = fn.apply(hintValue)
                  (tt.tpe.typeArgs.size > 0 && tt.tpe.typeArgs.head == foundType) || foundType.baseClasses.contains(tt.tpe.typeSymbol)
                } catch {
                  case _: Throwable => false // attempt to modify failed somehow
                }
              case None => Try(reader.jackFlavor.typeTypeAdapter.typeNameToType(Path.Root, hintValue, reader)) match {
                case Success(foundType) =>
                  (tt.tpe.typeArgs.size > 0 && tt.tpe.typeArgs.head == foundType) || foundType.baseClasses.contains(tt.tpe.typeSymbol)
                case _ => false
              }
            }
          case None => false
        }
        result
      } =>
        Some(_read(reader)(tt))

      case _ => None
    }
    Filter(partialFunc)
  }
}

