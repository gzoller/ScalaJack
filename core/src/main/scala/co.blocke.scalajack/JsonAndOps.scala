package co.blocke.scalajack

trait JsonAndOps {
  type FieldsValueType
  type JsonType
  val capturedFields: FieldsValueType
  implicit val jsonOps: JsonOps[JsonType]
}

object JsonAndOps {
  def apply[J, OF](captured: OF)(implicit ops: JsonOps.Aux[J, OF]): JsonAndOps =
    new JsonAndOps {
      override type FieldsValueType = OF
      override type JsonType = J
      override val capturedFields: FieldsValueType = captured
      override implicit val jsonOps: JsonOps[J] = ops
    }

}

/*
trait JsonAndOps[J] {

  type FieldsValue

  val capturedFields: FieldsValue

  implicit val jsonOps: JsonOps[J]

}

object JsonAndOps {

  def apply[J](saveTheseFields: Any)(implicit ops: JsonOps[J]): JsonAndOps[J] =
    new JsonAndOps[J] {

      override type FieldsValue = ops.ObjectFields

      override val capturedFields: FieldsValue = saveTheseFields.asInstanceOf[FieldsValue]

      override implicit val jsonOps: JsonOps[J] = ops

    }

}
*/
