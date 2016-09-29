package co.blocke.scalajack
package typeadapter

import scala.reflect.runtime.universe.{ ClassSymbol, Type, typeOf }

object EnumerationTypeAdapter extends TypeAdapterFactory.FromClassSymbol {

  override def typeAdapter(tpe: Type, classSymbol: ClassSymbol, context: Context): Option[TypeAdapter[_]] =
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

case class EnumerationTypeAdapter[E <: Enumeration](enum: E) extends TypeAdapter[E#Value] with StringKind {

  override def read(reader: Reader): E#Value =
    reader.peek match {
      case TokenType.String ⇒
        try {
          enum.withName(reader.readString())
        } catch {
          case nse: java.util.NoSuchElementException ⇒ throw new java.util.NoSuchElementException(s"No value found in enumeration ${enum.getClass.getName} for '${reader.tokenText}'" + "\n" + reader.showError())
        }
      case TokenType.Null ⇒ reader.readNull()
      case actual ⇒
        reader.read()
        throw new IllegalStateException(s"Expected value token of type String, not $actual when reading Enumeration value.\n" + reader.showError())
    }

  override def write(value: E#Value, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString(value.toString)
    }
}
