package co.blocke.scalajack

import org.apache.commons.text.StringEscapeUtils.escapeJava

object JsonRenderer {

  def renderCompact[J](json: J, sj: ScalaJackLike[_, _])(implicit ops: JsonOps[J]): String = {
    val builder = new StringBuilder

    def appendString(string: String): Unit = {
      var i = 0
      val length = string.length

      var beginIndex = 0

      if (sj.isCanonical) {
        builder.append('"' + string + '"')
      } else {
        string match {
          case "" => builder.append("\"\"")
          case s =>
            string.charAt(i) match {
              case '{' | '[' =>
                builder.appendAll(string)
                beginIndex = i
                i += 1

              case No_Quote_Marker => // no-quotes rendering
                builder.appendAll(string.tail)
                beginIndex = i
                i += 1

              case _ =>
                builder.append('"')
                i += 1
                builder.appendAll(string.substring(beginIndex))
                builder.append('"')
            }
        }
      }
    }

    def helper(json: J): Unit =
      json match {
        case JsonArray(x) =>
          builder.append('[')
          ops.foreachArrayElement(x.asInstanceOf[ops.ArrayElements], { (index, element) =>
            if (index > 0) {
              builder.append(",")
            }
            helper(element)
          })
          builder.append(']')

        case JsonBoolean(booleanValue) =>
          builder.append(if (booleanValue) "true" else "false")

        case JsonDecimal(bigDecimal) =>
          builder.append(bigDecimal.toString)

        case JsonDouble(doubleValue) =>
          builder.append(doubleValue)

        case JsonInt(bigInt) =>
          builder.append(bigInt.toString)

        case JsonLong(longValue) =>
          builder.append(longValue)

        case JsonNull() =>
          builder.append("null")

        case JsonObject(x) =>
          builder.append('{')
          var isFirst = true
          ops.foreachObjectField(x.asInstanceOf[ops.ObjectFields], { (name, value) =>
            if (isFirst) {
              isFirst = false
            } else {
              builder.append(",")
            }
            if (sj.isCanonical)
              appendString(escapeJava(name))
            else {
              appendString(name)
            }
            builder.append(":")
            helper(value)
          })
          builder.append('}')

        case JsonString(string) =>
          appendString(escapeJava(string))
      }

    helper(json)

    builder.result()
  }
}
