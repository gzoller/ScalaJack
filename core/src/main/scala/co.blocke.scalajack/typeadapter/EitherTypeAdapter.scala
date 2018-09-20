package co.blocke.scalajack
package typeadapter

object EitherTypeAdapter extends TypeAdapterFactory.=:=.withTwoTypeParams[Either] {

  override def create[L, R](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[Either[L, R]], ttLeft: TypeTag[L], ttRight: TypeTag[R]): TypeAdapter[Either[L, R]] = {
    val leftType = ttLeft.tpe
    val rightType = ttRight.tpe

    if (leftType <:< rightType || rightType <:< leftType) {
      throw new IllegalArgumentException(s"Types $leftType and $rightType are not mutually exclusive")
    }

    val leftTypeAdapter = context.typeAdapterOf[L]
    val rightTypeAdapter = context.typeAdapterOf[R]

    EitherTypeAdapter(
      new EitherDeserializer(leftTypeAdapter.deserializer, rightTypeAdapter.deserializer),
      new EitherSerializer(leftTypeAdapter.serializer, rightTypeAdapter.serializer),
      leftTypeAdapter,
      rightTypeAdapter)
  }

}

case class EitherTypeAdapter[L, R](override val deserializer: Deserializer[Either[L, R]], override val serializer: Serializer[Either[L, R]], leftTypeAdapter: TypeAdapter[L], rightTypeAdapter: TypeAdapter[R]) extends TypeAdapter[Either[L, R]]
