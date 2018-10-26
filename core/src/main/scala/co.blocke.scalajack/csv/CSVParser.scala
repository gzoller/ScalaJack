package co.blocke.scalajack
package csv

import org.apache.commons.text.StringTokenizer

import scala.collection.JavaConverters._

object CSVParser extends Parser[String] {

  def parse[AST](source: String)(implicit ops: AstOps[AST, String]): Option[AST] =
    if (source.trim == "")
      Some(AstNull())
    else {
      val found = StringTokenizer.getCSVInstance(source).getTokenList.asScala.toList.map(inferKind(_))
      val result = found.foldLeft(Some(List.empty[AST]).asInstanceOf[Option[List[AST]]]) {
        case (acc, item) => if (item.isEmpty || acc.isEmpty) None else acc.map(_ :+ item.get)
      }
      result.map(r =>
        ops.applyArray { appendElement =>
          r.foreach(appendElement)
        })
    }

  // Infers Boolean, Double, Long, String, or Null
  private def inferKind[AST](s: String)(implicit ops: AstOps[AST, String]): Option[AST] = {
    val trimmed = s.trim
    trimmed match {
      case "true" | "false" => Some(ops.applyBoolean(trimmed.toBoolean))
      case ""               => Some(ops.applyNull())
      case IsLong(l)        => Some(ops.applyLong(l))
      case IsDouble(d)      => Some(ops.applyDouble(d))
      case _                => Some(ops.applyString(trimmed))
    }
  }
}

object IsDouble {
  def unapply(s: String): Option[Double] =
    try {
      Some(s.toDouble)
    } catch {
      case _: NumberFormatException => None
    }
}

object IsLong {
  def unapply(s: String): Option[Long] =
    try {
      Some(s.toLong)
    } catch {
      case _: NumberFormatException => None
    }
}