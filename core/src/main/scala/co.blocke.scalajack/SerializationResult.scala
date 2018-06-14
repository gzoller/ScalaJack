package co.blocke.scalajack

import scala.collection.immutable

sealed trait SerializationResult[+J] {

  def get: J

  def map[JJ](f: J => JJ): SerializationResult[JJ]

}

case class SerializationSuccess[+J](get: J) extends SerializationResult[J] {

  override def map[JJ](f: J => JJ): SerializationResult[JJ] =
    SerializationSuccess(f(get))

}

object SerializationFailure {

  def apply[J](errors: SerializationError*)(implicit ops: JsonOps[J]): SerializationFailure[J] =
    new SerializationFailure[J](errors.to[immutable.Seq])

}

case class SerializationFailure[+J](errors: immutable.Seq[SerializationError]) extends SerializationResult[J] {

  override def get: J = throw new UnsupportedOperationException("SerializationFailure.get not supported")

  override def map[JJ](f: J => JJ): SerializationResult[JJ] = this.asInstanceOf[SerializationResult[JJ]]

}

sealed trait SerializationError

object SerializationError {

  case object Nothing extends SerializationError

}
