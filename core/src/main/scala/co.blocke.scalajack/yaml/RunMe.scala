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

case class AnyShell(a: Any)

object RunMe extends App {

  val sj = ScalaJack(YamlFlavor())

  val yaml2 = """a: null"""

  try {
    val z = sj.read[AnyShell](yaml2)
    println(z)
  } catch {
    case _ => println("*** Boom! ***")
  }
}
