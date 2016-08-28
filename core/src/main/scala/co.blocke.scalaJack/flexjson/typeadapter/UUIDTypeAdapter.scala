package co.blocke.scalajack.flexjson.typeadapter

import java.util.UUID

import co.blocke.scalajack.flexjson.{Reader, Writer}

object UUIDTypeAdapter extends SimpleTypeAdapter[UUID] {

  override def read(reader: Reader): UUID =
    UUID.fromString(reader.readString())

  override def write(value: UUID, writer: Writer): Unit = ???

}
