package co.blocke.scalajack

trait AstAndOps {
  type FieldsValueType
  type ASTType
  type SrcType
  val capturedFields: FieldsValueType
  implicit val astOps: AstOps[ASTType, SrcType]
}

object AstAndOps {
  def apply[AST, OF, S](captured: OF)(implicit ops: AstOps.Aux[AST, OF, S]): AstAndOps =
    new AstAndOps {
      override type FieldsValueType = OF
      override type ASTType = AST
      override type SrcType = S
      override val capturedFields: FieldsValueType = captured
      override implicit val astOps: AstOps[ASTType, SrcType] = ops
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
