package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{ Reader, Writer }

import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.Type

object TypeTypeAdapter extends SimpleTypeAdapter[Type] {

  override def read(reader: Reader): Type = {
    val fullName = reader.readString()
    try {
      currentMirror.staticClass(fullName).info
    } catch {
      case e: ScalaReflectionException ⇒
        throw new ClassNotFoundException(s"""Unable to find class named "$fullName"""", e)
    }
  }

  override def write(value: Type, writer: Writer): Unit = {
    val fullName = value.typeSymbol.fullName
    writer.writeString(fullName)
  }

}
