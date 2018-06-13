package co.blocke.scalajack

object JsonRenderer {

  def renderCompact[J](json: J)(implicit ops: JsonOps[J]): String = {
    val builder = new StringBuilder

    def appendString(string: String): Unit = {
      builder.append('"').append(string).append('"') // FIXME escape characters
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
          ops.foreachObjectField(x.asInstanceOf[ops.ObjectFields], { (name, value) =>
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
