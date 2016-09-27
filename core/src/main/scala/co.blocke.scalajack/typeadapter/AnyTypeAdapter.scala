package co.blocke.scalajack
package typeadapter

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.{ Type, typeOf }
import scala.reflect.runtime.currentMirror

object AnyTypeAdapter extends TypeAdapterFactory {

  override def typeAdapter(tpe: Type, context: Context): Option[TypeAdapter[_]] =
    if (tpe =:= typeOf[Any]) {
      val typeTypeAdapter = context.typeAdapterOf[Type]
      val memberNameTypeAdapter = context.typeAdapterOf[MemberName]
      val mapTypeAdapter = context.typeAdapterOf[Map[Any, Any]]
      val listTypeAdapter = context.typeAdapterOf[List[Any]]
      val stringTypeAdapter = context.typeAdapterOf[String]
      val booleanTypeAdapter = context.typeAdapterOf[Boolean]

      Some(AnyTypeAdapter(typeTypeAdapter, memberNameTypeAdapter, mapTypeAdapter, listTypeAdapter, stringTypeAdapter, booleanTypeAdapter, context))
    } else {
      None
    }

}

case class AnyTypeAdapter(
    typeTypeAdapter:       TypeAdapter[Type],
    memberNameTypeAdapter: TypeAdapter[MemberName],
    mapTypeAdapter:        TypeAdapter[Map[Any, Any]],
    listTypeAdapter:       TypeAdapter[List[Any]],
    stringTypeAdapter:     TypeAdapter[String],
    booleanTypeAdapter:    TypeAdapter[Boolean],
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
        reader.readNumber() // Use Scala numerical inference (see Reader.readNumber())

      case TokenType.Null ⇒
        reader.readNull()

    }
  }

  override def write(value: Any, writer: Writer): Unit = {
    // TODO come up with a better way to obtain the value's type

    value match {
      case null ⇒
        writer.writeNull()

      case string: String ⇒
        stringTypeAdapter.write(string, writer)

      case enum: Enumeration#Value ⇒
        stringTypeAdapter.write(enum.toString, writer)

      case list: List[_] ⇒
        listTypeAdapter.write(list, writer)

      case map: Map[_, _] ⇒
        mapTypeAdapter.write(map.asInstanceOf[Map[Any, Any]], writer)

      case _ ⇒
        val valueType = currentMirror.staticClass(value.getClass.getName).toType
        //    val valueType = currentMirror.reflectClass(currentMirror.classSymbol(value.getClass)).symbol.info
        //    val valueType = currentMirror.reflect(value)(ClassTag(value.getClass)).symbol.info

        val valueTypeAdapter = context.typeAdapter(valueType)

        val polymorphicWriter = new PolymorphicWriter(writer, "_hint", valueType, typeTypeAdapter, memberNameTypeAdapter)

        valueTypeAdapter.asInstanceOf[TypeAdapter[Any]].write(value, polymorphicWriter)
    }
  }

}
