package co.blocke.scalajack
package typeadapter

object Thing {

  def main(args: Array[String]): Unit = {
    TupleDeserializerUsingCompilation.apply(2)
  }

}

object TupleDeserializerUsingCompilation {

  def apply[Tuple](size: Int): Deserializer[Tuple] = {
    generateClass(size)
    null
  }

  def generateClass(size: Int): Class[_] = {
    val deserializerClassName = s"Tuple${size}Deserializer"

    ScalaCompiler.compileClass(
      s"""
        |import co.blocke.{ scalajack => sj }
        |
        |class $deserializerClassName[${(1 to size).map(i => s"T$i").mkString(", ")}](${(1 to size).map(i => s"deserializer$i: sj.Deserializer[T$i]").mkString(", ")})(implicit ${(1 to size).map(i => s"tt$i: sj.TypeTag[T$i]").mkString(", ")}) extends sj.typeadapter.TupleDeserializerUsingCompilation[(${(1 to size).map(i => s"T$i").mkString(", ")})] {
        |
        |  private val tupleTypeConstructor: sj.Type = sj.typeOf[(${(1 to size).map(i => "_").mkString(", ")})].typeConstructor
        |  private val taggedNull: sj.TypeTagged[(${(1 to size).map(i => s"T$i").mkString(", ")})] = sj.TypeTagged[(${(1 to size).map(i => s"T$i").mkString(", ")})](null, sj.appliedType(tupleTypeConstructor, List(${(1 to size).map(i => s"tt$i.tpe").mkString(", ")})))
        |
        |  private class TaggedTuple(override val get: (${(1 to size).map(i => s"T$i").mkString(", ")}), ${(1 to size).map(i => s"taggedElement$i: sj.TypeTagged[T$i]").mkString(", ")}) extends sj.TypeTagged[(${(1 to size).map(i => s"T$i").mkString(", ")})] {
        |    override lazy val tpe: sj.Type = sj.appliedType(tupleTypeConstructor, List(${(1 to size).map(i => s"taggedElement$i.tpe").mkString(", ")}))
        |  }
        |
        |  override def deserialize[AST,S](path: sj.Path, ast: AST)(implicit sj.AstOps[AST,S], guidance: SerializationGuidance): sj.DeserializationResult[(${(1 to size).map(i => s"T$i").mkString(", ")})] =
        |    json match {
        |      case sj.AstNull() => sj.DeserializationSuccess(sj.TypeTagged(null, tupleType))
        |      case sj.AstArray(x) =>
        |${(1 to size).map(i => s"""        var deserializationResult$i: sj.DeserializationResult[T$i] = null""").mkString("\n")}
        |
        |        val errorsBuilder = scala.collection.immutable.Seq.newBuilder[(sj.Path, sj.DeserializationError)]
        |
        |        ops.foreachArrayElement(x.asInstanceOf[ops.ArrayElements], { (index, elementAst) =>
        |          index match {
        |${(1 to size).map(i => s"""            case ${i - 1} => deserializationResult$i = deserializer$i.deserialize(path \\ index, elementAst); errorsBuilder ++= deserializationResult$i.errors""").mkString("\n")}
        |            case _ => errorsBuilder += (path \\ index, sj.DeserializationError.Unexpected("A tuple of size $size must not have an element at index " + index))
        |          }
        |        })
        |
        |${(1 to size).map(i => s"""        if (deserializationResult$i eq null) { deserializationResult$i = deserializer$i.deserializeFromNothing[AST,S](path \\ ${i - 1}); errors ++= deserializationResult$i.errors }""").mkString("\n")}
        |
        |        val errors = errorsBuilder.result()
        |
        |        if (errors.nonEmpty || ${(1 to size).map(i => s"deserializationResult$i.isFailure").mkString(" || ")}) {
        |          sj.DeserializationFailure(errors)
        |        } else {
        |${(1 to size).map(i => s"""          val sj.DeserializationSuccess(taggedElement$i @ sj.TypeTagged(element$i)) = deserializationResult$i""").mkString("\n")}
        |
        |          new TaggedTuple((${(1 to size).map(i => s"element$i").mkString(", ")}), ${(1 to size).map(i => s"taggedElement$i").mkString(", ")})
        |        }
        |    }
        |
        |}
      """.stripMargin)
  }

}

trait TupleDeserializerUsingCompilation[Tuple] extends Deserializer[Tuple] {

}
