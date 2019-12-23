package co.blocke.scalajack
package yaml

import org.snakeyaml.engine.v2.api.{DumpSettings, LoadSettings}
import org.snakeyaml.engine.v2.api.lowlevel._
import org.snakeyaml.engine.v2.nodes._
import org.snakeyaml.engine.v2.common._
import org.snakeyaml.engine.v2.parser.ParserImpl
import org.snakeyaml.engine.v2.scanner.StreamReader

import scala.jdk.CollectionConverters._

import scala.util._

class Embed() {
  var stuff: List[String] = List.empty[String]
  var num: Int            = 0
}

class Boom() {
  var name: String      = ""
  var other: Try[Embed] = Success(null)
}

object RunMe extends App {

  val sj = ScalaJack(YamlFlavor())

  val yaml =
    """name: Greg
      |other: [1, 2, 3]""".stripMargin

  println(sj.read[Boom](yaml))
}
