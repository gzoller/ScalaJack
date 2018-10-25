package co.blocke.scalajack
package json

import org.apache.commons.text.StringEscapeUtils.escapeJava

object JsonRenderer extends Renderer[String] {

  def renderCompact[AST](ast: AST, sj: ScalaJackLike[_, _])(implicit ops: AstOps[AST, String]): String = {
    val builder = new StringBuilder

    def appendString(string: String): Unit = {
      var i = 0

      var beginIndex = 0

      if (sj.isCanonical) {
        builder.append('"' + string + '"')
      } else {
        string match {
          case "" => builder.append("\"\"")
          case _ =>
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

    def helper(ast: AST): Unit =
      ast match {
        case AstArray(x) =>
          builder.append('[')
          ops.foreachArrayElement(x.asInstanceOf[ops.ArrayElements], { (index, element) =>
            if (index > 0) {
              builder.append(",")
            }
            helper(element)
          })
          builder.append(']')

        case AstBoolean(booleanValue) =>
          builder.append(if (booleanValue) "true" else "false")

        case AstDecimal(bigDecimal) =>
          builder.append(bigDecimal.toString)

        case AstDouble(doubleValue) =>
          builder.append(doubleValue)

        case AstInt(bigInt) =>
          builder.append(bigInt.toString)

        case AstLong(longValue) =>
          builder.append(longValue)

        case AstNull() =>
          builder.append("null")

        case AstObject(x) =>
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

        case AstString(string) =>
          appendString(escapeJava(string))
      }

    helper(ast)

    builder.result()
  }
}
