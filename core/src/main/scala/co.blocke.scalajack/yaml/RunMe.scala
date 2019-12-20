package co.blocke.scalajack
package yaml

import org.snakeyaml.engine.v2.api.{DumpSettings, LoadSettings}
import org.snakeyaml.engine.v2.api.lowlevel._
import org.snakeyaml.engine.v2.nodes._
import org.snakeyaml.engine.v2.common._
import org.snakeyaml.engine.v2.parser.ParserImpl
import org.snakeyaml.engine.v2.scanner.StreamReader

import scala.jdk.CollectionConverters._

case class Person(name: String, age: List[Int]) extends SJCapture

case class SampleByte(b1: Byte, b2: Byte, b3: Byte, b4: Byte)

object RunMe extends App {

  val sj = ScalaJack(YamlFlavor())

  val yaml2 =
    """1.2""".stripMargin

  val z = sj.read[Short](yaml2)
  println(z)
}
