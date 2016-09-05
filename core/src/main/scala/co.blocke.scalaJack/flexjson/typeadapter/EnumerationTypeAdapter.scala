package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{ Context, Reader, TokenType, TypeAdapter, TypeAdapterFactory, Writer }
import scala.reflect.runtime.universe.{ ClassSymbol, Type, typeOf }

object EnumerationTypeAdapter extends TypeAdapterFactory.FromClassSymbol {

  override def typeAdapter(tpe: Type, classSymbol: ClassSymbol, context: Context, superParamTypes: List[Type]): Option[TypeAdapter[_]] =
    if (tpe.typeSymbol.fullName == "scala.Enumeration.Value") { // Can't use tpe <:< because Enumeration has no companion object
      val erasedEnumClassName = tpe.toString match {
        case raw if (raw.endsWith(".Value")) ⇒ raw.replace(".Value", "$")
        case raw                             ⇒ raw.dropRight(raw.length - raw.lastIndexOf('.')) + "$"
      }
      val enum = Class.forName(erasedEnumClassName).getField(scala.reflect.NameTransformer.MODULE_INSTANCE_NAME).get(null).asInstanceOf[Enumeration]
      Some(EnumerationTypeAdapter(enum))
    } else {
      None
    }
}

case class EnumerationTypeAdapter[E <: Enumeration](enum: E) extends TypeAdapter[E#Value] {

  override def read(reader: Reader): E#Value =
    reader.peek match {
      case TokenType.String ⇒ enum.withName(reader.readString())
      case TokenType.Null   ⇒ reader.readNull()
    }

  override def write(value: E#Value, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value.toString)
    }
}
