package co.blocke.scalajack
package schema

import model._
import typeadapter.classes._
import SchemaType._

case class SchemaEntryFactory(ctx: Context) extends java.util.function.Function[Type, JsonSchema] {
  override def apply(tpe: Type): JsonSchema = {
    ctx.typeAdapter(tpe) match {
      case ccta: CaseClassTypeAdapter[_] =>
        val id = ClassHelper.getAnnotationValue[SchemaId, String](tpe.typeSymbol)
        JsonSchema(`object`, ccta.className, id)
      case _ => throw new Exception("Boom")
    }
  }
}
