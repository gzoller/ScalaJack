package co.blocke.scalajack
package json

import java.nio.ByteBuffer
import scala.specialized as sp

package object exp {

  final private[this] val readerPool: ThreadLocal[JsonReader] = new ThreadLocal[JsonReader] {
    override def initialValue(): JsonReader = new JsonReader
  }

  def readFromString(s: String): String =
    readerPool.get.read(s)
}
