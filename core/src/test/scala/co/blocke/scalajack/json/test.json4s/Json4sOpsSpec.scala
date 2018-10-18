package co.blocke.scalajack
package json
package test.json4s

import org.json4s.JsonAST.JValue

class Json4sOpsSpec extends JsonOpsSpec[JValue] {

  override val ops: JsonOps[JValue] = Json4sOps

}
