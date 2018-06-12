package co.blocke.scalajack

import scala.collection.immutable

sealed trait SerializationResult[+J] {

  def get: J

}

case class SerializationSuccess[+J](get: J) extends SerializationResult[J]

object SerializationFailure {

  def apply[J](errors: SerializationError*)(implicit ops: JsonOps[J]): SerializationFailure[J] =
    new SerializationFailure[J](errors.to[immutable.Seq])

}

case class SerializationFailure[+J](errors: immutable.Seq[SerializationError]) extends SerializationResult[J] {

  override def get: J = throw new UnsupportedOperationException("SerializationFailure.get not supported")

}

sealed trait SerializationError

object SerializationError {

  case object Nothing extends SerializationError

}
