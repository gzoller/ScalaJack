package co.blocke.scalajack

import scala.collection.immutable

sealed trait SerializationResult[+J] {

  def get: J

  def map[JJ](f: J => JJ): SerializationResult[JJ]

  def errors: immutable.Seq[SerializationError]

}

case class SerializationSuccess[+J](get: J) extends SerializationResult[J] {

  override def map[JJ](f: J => JJ): SerializationResult[JJ] =
    SerializationSuccess(f(get))

  override def errors: immutable.Seq[SerializationError] = immutable.Seq.empty

}

object SerializationFailure {

  def apply[AST, S](errors: SerializationError*)(implicit ops: AstOps[AST, S]): SerializationFailure[AST] =
    new SerializationFailure[AST](errors.to[immutable.Seq])

}

case class SerializationFailure[+J](errors: immutable.Seq[SerializationError]) extends SerializationResult[J] {

  override def get: J = throw new UnsupportedOperationException("SerializationFailure.get not supported")

  override def map[JJ](f: J => JJ): SerializationResult[JJ] = this.asInstanceOf[SerializationResult[JJ]]

  def isNothing: Boolean =
    errors.contains(SerializationError.Nothing)

  override def toString: String =
    productPrefix + errors.mkString("(", ", ", ")")

}

sealed trait SerializationError

object SerializationError {

  case object Nothing extends SerializationError

  case class ExceptionThrown(exception: Throwable) extends SerializationError

}
