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

  case class Unsupported(message: String, reportedBy: Option[Deserializer[_]] /* = None*/ ) extends DeserializationError {

    override def toString: String = s"$message${reportedBy.map(rb => s" (reported by: $rb)").getOrElse("")}"

  }

  case class Unexpected(message: String) extends DeserializationError

  object Malformed {

    def apply(cause: Throwable): DeserializationError =
      new Malformed(message = cause.getMessage)

  }

  case class Malformed(message: String) extends DeserializationError

}
