package co.blocke.scalajack.json
package typeadapter

import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.Type

object TypeTypeAdapter extends SimpleTypeAdapter[Type] {

  override def read(reader: Reader): Type = {
    val fullName = reader.readString()
    try {
      val t1 = currentMirror.staticClass(fullName).info
      val t2 = currentMirror.staticClass(fullName).toType
      t2
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
