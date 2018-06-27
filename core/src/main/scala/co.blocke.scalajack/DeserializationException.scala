package co.blocke.scalajack

import co.blocke.scalajack.typeadapter.BackedByJsonValue

object DeserializationException {

  private def format(deserializationFailure: DeserializationFailure): String = {
    val stringBuilder = new StringBuilder

    val errors = deserializationFailure.errors

    stringBuilder.append("DeserializationException")

    errors.size match {
      case 0 =>
        stringBuilder.append("(no errors)")
      case 1 =>
        stringBuilder.append("(1 error):")
      case n =>
        stringBuilder.append(s"($n errors):")
    }

    for ((path, error) <- errors) {
      stringBuilder.append(s"\n  [$path] $error")
    }

    stringBuilder.result()
  }

}

class DeserializationException(val deserializationFailure: DeserializationFailure) extends RuntimeException(DeserializationException.format(deserializationFailure))
