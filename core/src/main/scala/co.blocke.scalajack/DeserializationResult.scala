package co.blocke.scalajack

import scala.collection.immutable
import scala.util.control.NonFatal

object DeserializationResult {

  def apply[T](path: Path)(body: => TypeTagged[T], deserializationError: PartialFunction[Throwable, DeserializationError] = PartialFunction.empty): DeserializationResult[T] =
    try {
      DeserializationSuccess(body)
    } catch {
      case NonFatal(e) =>
        DeserializationFailure(immutable.Seq((path, deserializationError.applyOrElse(e, DeserializationError.ExceptionThrown))))
    }

}

sealed trait DeserializationResult[+T] {

  def get: TypeTagged[T]

  def errors: immutable.Seq[(Path, DeserializationError)]

  def isSuccess: Boolean

  def isFailure: Boolean

}

case class DeserializationSuccess[+T](get: TypeTagged[T]) extends DeserializationResult[T] {

  override def errors: immutable.Seq[(Path, DeserializationError)] = immutable.Seq.empty

  override def isSuccess: Boolean = true

  override def isFailure: Boolean = false

}

object DeserializationFailure {

  def apply(path: Path, errors: DeserializationError*): DeserializationFailure =
    new DeserializationFailure(errors.map(error => (path, error)).to[immutable.Seq])

}

case class DeserializationFailure(errors: immutable.Seq[(Path, DeserializationError)]) extends DeserializationResult[Nothing] {

  override def get: TypeTagged[Nothing] = throw new UnsupportedOperationException("DeserializationFailure.get")

  override def isSuccess: Boolean = false

  override def isFailure: Boolean = true

}
