package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{ Context, Reader, TokenType, TypeAdapter, TypeAdapterFactory, Writer }

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.{ Type, typeOf }
import scala.reflect.runtime.currentMirror

object AnyTypeAdapter extends TypeAdapterFactory {

  override def typeAdapter(tpe: Type, context: Context): Option[TypeAdapter[_]] =
    if (tpe =:= typeOf[Any]) {
      val mapTypeAdapter = context.typeAdapterOf[Map[String, Any]]
      val listTypeAdapter = context.typeAdapterOf[List[Any]]
      val stringTypeAdapter = context.typeAdapterOf[String]
      val booleanTypeAdapter = context.typeAdapterOf[Boolean]
      val bigDecimalTypeAdapter = context.typeAdapterOf[BigDecimal]

      Some(AnyTypeAdapter(mapTypeAdapter, listTypeAdapter, stringTypeAdapter, booleanTypeAdapter, bigDecimalTypeAdapter, context))
    } else {
      None
    }

}

case class AnyTypeAdapter(
    mapTypeAdapter:        TypeAdapter[Map[String, Any]],
    listTypeAdapter:       TypeAdapter[List[Any]],
    stringTypeAdapter:     TypeAdapter[String],
    booleanTypeAdapter:    TypeAdapter[Boolean],
    bigDecimalTypeAdapter: TypeAdapter[BigDecimal],
    context:               Context
) extends SimpleTypeAdapter[Any] {

  override def read(reader: Reader): Any = {
    reader.peek match {
      case TokenType.BeginObject ⇒
        mapTypeAdapter.read(reader)

      case TokenType.BeginArray ⇒
        listTypeAdapter.read(reader)

      case TokenType.String ⇒
        stringTypeAdapter.read(reader)

      case TokenType.True | TokenType.False ⇒
        booleanTypeAdapter.read(reader)

      case TokenType.Number ⇒
        bigDecimalTypeAdapter.read(reader)

      case TokenType.Null ⇒
        reader.readNull()

    }
  }

  override def write(value: Any, writer: Writer): Unit = {
    // TODO come up with a better way to obtain the value's type

    value match {
      case string: String ⇒
        context.typeAdapterOf[String].write(string, writer)

      case _ ⇒
        val valueType = currentMirror.staticClass(value.getClass.getName).info
        //    val valueType = currentMirror.reflectClass(currentMirror.classSymbol(value.getClass)).symbol.info
        //    val valueType = currentMirror.reflect(value)(ClassTag(value.getClass)).symbol.info

        context.typeAdapter(valueType).asInstanceOf[TypeAdapter[Any]].write(value, writer)
    }
  }

}
