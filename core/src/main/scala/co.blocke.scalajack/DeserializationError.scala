package co.blocke.scalajack

sealed trait DeserializationError

object DeserializationError {

  case object Missing extends DeserializationError

  case class ExceptionThrown(exception: Throwable) extends DeserializationError

  case class Unsupported(message: String) extends DeserializationError

}
