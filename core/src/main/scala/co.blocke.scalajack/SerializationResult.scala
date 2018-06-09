package co.blocke.scalajack

sealed trait SerializationResult[+J] {

  def get: J

}

case class SerializationSuccess[+J](get: J) extends SerializationResult[J]

case class SerializationFailure[+J]() extends SerializationResult[J] {

  override def get: J = throw new UnsupportedOperationException("SerializationFailure.get not supported")

}
