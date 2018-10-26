package co.blocke.scalajack
package csv

import co.blocke.scalajack.json.Json4sOpsBase

object CSVOps extends Json4sOpsBase {

  val parser: Parser[String] = CSVParser
  val renderer: Renderer[String] = CSVRenderer

}
