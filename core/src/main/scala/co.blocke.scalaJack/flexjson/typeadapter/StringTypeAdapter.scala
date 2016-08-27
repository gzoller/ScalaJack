package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{Reader, Writer}

object StringTypeAdapter extends SimpleTypeAdapter[String] {

  override def read(reader: Reader): String = {
    reader.readString()
  }

  override def write(value: String, writer: Writer): Unit = ???

}
