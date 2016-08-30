package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{Context, Reader, TokenType, TypeAdapter, TypeAdapterFactory, Writer}

import scala.reflect.runtime.universe.{Type, typeOf}

object AnyTypeAdapter extends TypeAdapterFactory {

  override def typeAdapter(tpe: Type, context: Context): Option[TypeAdapter[_]] =
    if (tpe =:= typeOf[Any]) {
      val mapTypeAdapter = context.typeAdapterOf[Map[String, Any]]
      val listTypeAdapter = context.typeAdapterOf[List[Any]]
      val stringTypeAdapter = context.typeAdapterOf[String]
      val booleanTypeAdapter = context.typeAdapterOf[Boolean]

      Some(AnyTypeAdapter(mapTypeAdapter, listTypeAdapter, stringTypeAdapter, booleanTypeAdapter))
    } else {
      None
    }

}

case class AnyTypeAdapter(mapTypeAdapter: TypeAdapter[Map[String, Any]],
                          listTypeAdapter: TypeAdapter[List[Any]],
                          stringTypeAdapter: TypeAdapter[String],
                          booleanTypeAdapter: TypeAdapter[Boolean]) extends SimpleTypeAdapter[Any] {

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
    }
  }

  override def write(value: Any, writer: Writer): Unit = ???

}
