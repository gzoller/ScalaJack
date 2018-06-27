package co.blocke.scalajack

trait JsonAndOps {

  type JsonValue

  val jsonValue: JsonValue

  implicit val jsonOps: JsonOps[JsonValue]

}

object JsonAndOps {

  def apply[J](json: J)(implicit ops: JsonOps[J]): JsonAndOps =
    new JsonAndOps {

      override type JsonValue = J

      override val jsonValue: JsonValue = json

      override implicit val jsonOps: JsonOps[JsonValue] = ops

    }

}
