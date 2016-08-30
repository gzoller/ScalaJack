package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{Reader, Writer}

import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.Type

object TypeTypeAdapter extends SimpleTypeAdapter[Type] {

  override def read(reader: Reader): Type = {
    val fullName = reader.readString()
    currentMirror.staticClass(fullName).info
  }

  override def write(value: Type, writer: Writer): Unit = {
    val fullName = value.typeSymbol.fullName
    writer.writeString(fullName)
  }

}
