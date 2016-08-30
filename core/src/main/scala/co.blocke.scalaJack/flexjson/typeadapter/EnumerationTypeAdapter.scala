package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{ Context, Reader, TypeAdapter, TypeAdapterFactory, Writer }
import scala.reflect.runtime.universe.{ ClassSymbol, Type, typeOf }

object EnumerationTypeAdapter extends TypeAdapterFactory.FromClassSymbol {

  override def typeAdapter(tpe: Type, classSymbol: ClassSymbol, context: Context): Option[TypeAdapter[_]] =
    if (tpe.typeSymbol.fullName == "scala.Enumeration.Value") { // Can't use tpe <:< because Enumeration has no companion object
      val erasedEnumClassName = tpe.toString match {
        case raw if (raw.endsWith(".Value")) => raw.replace(".Value", "$")
        case raw                             => raw.dropRight(raw.length - raw.lastIndexOf('.')) + "$"
      }
      val enum = Class.forName(erasedEnumClassName).getField(scala.reflect.NameTransformer.MODULE_INSTANCE_NAME).get(null).asInstanceOf[Enumeration]
      Some(EnumerationTypeAdapter(enum))
    } else {
      None
    }
}

case class EnumerationTypeAdapter(enum: Enumeration) extends TypeAdapter[Enumeration#Value] {

  override def read(reader: Reader): Enumeration#Value =
    enum.withName(reader.readString())

  override def write(value: Enumeration#Value, writer: Writer): Unit = {
    // writer.writeChar(value)
  }

}
