package co.blocke.scalajack

object SerializationException {

  private def format(serializationFailure: SerializationFailure[_]): String = {
    val stringBuilder = new StringBuilder

    val errors = serializationFailure.errors

    stringBuilder.append("SerializationException")

    errors.size match {
      case 0 =>
        stringBuilder.append("(no errors)")
      case 1 =>
        stringBuilder.append("(1 error):")
      case n =>
        stringBuilder.append(s"($n errors):")
    }

    for (e <- errors) {
      stringBuilder.append(s"\n  $e")
    }

    stringBuilder.result()
  }

}

class SerializationException(val serializationFailure: SerializationFailure[_]) extends RuntimeException(SerializationException.format(serializationFailure))
