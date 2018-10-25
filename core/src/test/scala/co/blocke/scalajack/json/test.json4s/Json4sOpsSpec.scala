package co.blocke.scalajack
package json
package test.json4s

import org.json4s.JsonAST.JValue

class Json4sOpsSpec extends JsonOpsSpec[JValue, String] {

  override val ops: AstOps[JValue, String] = Json4sOps

}
