package co.blocke.scalajack

sealed trait DeserializationError

object DeserializationError {

  case class ExceptionThrown(exception: Throwable) extends DeserializationError

}
