package co.blocke.scalajack

object JsonRenderer {

  def renderCompact[J](json: J)(implicit ops: JsonOps[J]): String = {
    val builder = new StringBuilder

    def appendString(string: String): Unit = {
      var i = 0
      val length = string.length

      var beginIndex = 0

      builder.append('"')

      while (i < length) {
        string.charAt(i) match {
          case '"' =>
            builder.appendAll(string.substring(beginIndex, i))
            builder.append("""\"""")
            i += 1
            beginIndex = i

          case _ =>
            i += 1
        }
      }

      builder.appendAll(string.substring(beginIndex))

      builder.append('"')
    }

    def helper(json: J): Unit =
      json match {
        case JsonArray(x) =>
          builder.append('[')
          ops.foreachArrayElement(x.asInstanceOf[ops.ArrayElements], { (index, element) =>
            if (index > 0) {
              builder.append(", ")
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
              builder.append(", ")
            }
            appendString(name)
            builder.append(": ")
            helper(value)
          })
          builder.append('}')

        case JsonString(string) =>
          appendString(string)
      }

    helper(json)

    builder.result()
  }

}
