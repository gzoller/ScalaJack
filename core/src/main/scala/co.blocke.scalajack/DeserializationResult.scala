package co.blocke.scalajack

import scala.collection.immutable
import scala.util.control.NonFatal

object DeserializationResult {

  def apply[T](path: Path)(body: => TypeTagged[T], deserializationError: PartialFunction[Throwable, DeserializationError] = PartialFunction.empty): DeserializationResult[T] =
    try DeserializationSuccess(body) catch {
      case e: DeserializationException =>
        e.deserializationFailure
      case NonFatal(e) =>
        DeserializationFailure(path, deserializationError.applyOrElse(e, DeserializationError.ExceptionThrown))
    }

  def trapExceptions[T](path: Path)(body: => DeserializationResult[T], deserializationError: PartialFunction[Throwable, DeserializationError] = PartialFunction.empty): DeserializationResult[T] =
    try body catch {
      case e: DeserializationException =>
        e.deserializationFailure
      case NonFatal(e) =>
        DeserializationFailure(path, deserializationError.applyOrElse(e, DeserializationError.ExceptionThrown))
    }

}

sealed trait DeserializationResult[+T] {

  def get: TypeTagged[T]

  def errors: immutable.Seq[(Path, DeserializationError)]

  def map[U](f: TypeTagged[T] => TypeTagged[U]): DeserializationResult[U]

  def flatMap[U](f: TypeTagged[T] => DeserializationResult[U]): DeserializationResult[U]

  def isSuccess: Boolean

  def isFailure: Boolean

}

case class DeserializationSuccess[+T](get: TypeTagged[T]) extends DeserializationResult[T] {

  override def errors: immutable.Seq[(Path, DeserializationError)] = immutable.Seq.empty

  override def map[U](f: TypeTagged[T] => TypeTagged[U]): DeserializationResult[U] =
    DeserializationSuccess(f(get))

  override def flatMap[U](f: TypeTagged[T] => DeserializationResult[U]): DeserializationResult[U] =
    f(get)

  override def isSuccess: Boolean = true

  override def isFailure: Boolean = false

}

object DeserializationFailure {

  def apply(path: Path, errors: DeserializationError*): DeserializationFailure =
    new DeserializationFailure(errors.map(error => (path, error)).to[immutable.Seq])

}

case class DeserializationFailure(errors: immutable.Seq[(Path, DeserializationError)]) extends DeserializationResult[Nothing] {

  override def get: TypeTagged[Nothing] = throw new DeserializationException(this)

  override def map[U](f: TypeTagged[Nothing] => TypeTagged[U]): DeserializationResult[U] =
    this.asInstanceOf[DeserializationResult[U]]

  override def flatMap[U](f: TypeTagged[Nothing] => DeserializationResult[U]): DeserializationResult[U] =
    this.asInstanceOf[DeserializationResult[U]]

  override def isSuccess: Boolean = false

  override def isFailure: Boolean = true

  def errors(path: Path): immutable.Seq[(Path, DeserializationError)] = errors.filter(_._1 == path)

  def isUnsupported(path: Path): Boolean = errors(path).exists(_._2.isInstanceOf[DeserializationError.Unsupported])
  def isUnexpected(path: Path): Boolean = errors(path).exists(_._2.isInstanceOf[DeserializationError.Unexpected])

}
