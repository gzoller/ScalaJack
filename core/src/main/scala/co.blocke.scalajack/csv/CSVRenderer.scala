package co.blocke.scalajack
package csv

import scala.collection.mutable.StringBuilder
import org.apache.commons.text.StringEscapeUtils

object CSVRenderer extends Renderer[String] {
  def renderCompact[AST](ast: AST, sj: ScalaJackLike[_, _])(implicit ops: AstOps[AST, String]): String =
    ast match {
      case AstNull => ""
      case AstArray(a) =>
        val builder = new StringBuilder()
        ops.foreachArrayElement(a.asInstanceOf[ops.ArrayElements], { (index, element) =>
          if (index > 0) {
            builder.append(",")
          }
          builder.append(fieldRender(element))
        })
        builder.result
    }

  private def fieldRender[AST](ast: AST)(implicit ops: AstOps[AST, String]): String =
    StringEscapeUtils.escapeCsv(ast match {
      case AstNull()     => ""
      case AstString(s)  => s
      case AstLong(l)    => l.toString
      case AstInt(i)     => i.toString
      case AstDouble(d)  => d.toString
      case AstBoolean(b) => b.toString
      case AstDecimal(d) => d.toString
      case x =>
        throw new SerializationException(SerializationFailure(SerializationError.ExceptionThrown(new UnsupportedOperationException(s"CSV serialization of a field of type ${x.getClass.getSimpleName} is unsupported."))))
    })
}