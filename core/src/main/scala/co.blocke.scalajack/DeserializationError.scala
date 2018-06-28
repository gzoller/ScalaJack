package co.blocke.scalajack

sealed trait DeserializationError {

  def message: String

}

object DeserializationError {

  case object Missing extends DeserializationError {
    override def message: String = "Field was missing"
  }

  case class ExceptionThrown(exception: Throwable) extends DeserializationError {
    override def message: String = s"Exception was thrown: $exception"
  }

  case class Unsupported(message: String, reportedBy: Deserializer[_]) extends DeserializationError {

    override def toString: String = s"$message (reported by: $reportedBy)"

  }

  case class Unexpected(message: String) extends DeserializationError

  object Malformed {

    def apply(cause: Throwable, reportedBy: Deserializer[_]): DeserializationError =
      new Malformed(message    = cause.getMessage, reportedBy = reportedBy)

  }

  case class Malformed(message: String, reportedBy: Deserializer[_]) extends DeserializationError

}
