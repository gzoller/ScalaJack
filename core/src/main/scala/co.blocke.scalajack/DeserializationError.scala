package co.blocke.scalajack

sealed trait DeserializationError

object DeserializationError {

  case object Missing extends DeserializationError

  case class ExceptionThrown(exception: Throwable) extends DeserializationError

  case class Unsupported(message: String) extends DeserializationError

  case class Unexpected(message: String) extends DeserializationError

  object Malformed {

    def apply(cause: Throwable): DeserializationError =
      new Malformed(message = cause.getMessage)

  }

  case class Malformed(message: String) extends DeserializationError

}
