package co.blocke.scalajack
package json

import model.EmitterState
import org.apache.commons.text.StringEscapeUtils.escapeJava

//trait JsonEmitter { // Wire writer
//  def emit[String](prim: AST_PRIMITIVE, es: EmitterState[String]): String = es.emit(prim, es, false).result
//}

case class JsonEmitterState() extends EmitterState[String] {

  private val builder = new StringBuilder()

  def emit(prim: AST_PRIMITIVE, isMapKey: Boolean = false): EmitterState[String] = {
    prim match {
      case a: List[AST_PRIMITIVE]               => addArray(a, isMapKey)
      case b: Boolean                           => addBoolean(b, isMapKey)
      case i: BigInt                            => addInt(i, isMapKey)
      case d: BigDecimal                        => addDecimal(d, isMapKey)
      case m: Map[AST_PRIMITIVE, AST_PRIMITIVE] => addMap(m, isMapKey)
      case null                                 => addNull(isMapKey)
      case s: String                            => addString(s, isMapKey)
    }
  }

  def addArray(a: List[AST_PRIMITIVE], isMapKey: Boolean): EmitterState[String] = {
    val b = if (isMapKey) new StringBuilder else builder
    b.append("[")
    var first = true
    a.foreach { e =>
      if (!first)
        b.append(',')
      first = false
      emit(e)
    }
    b.append("]")
    if (isMapKey)
      builder.append(escapeJava(b.result))
    this
  }

  def addBoolean(b: Boolean, isMapKey: Boolean): EmitterState[String] = {
    if (isMapKey)
      builder.append('"' + b.toString + '"')
    else
      builder.append(b.toString)
    this
  }

  def addInt(i: BigInt, isMapKey: Boolean): EmitterState[String] = {
    if (isMapKey)
      builder.append('"' + i.toString + '"')
    else
      builder.append(i.toString)
    this
  }

  def addDecimal(d: BigDecimal, isMapKey: Boolean): EmitterState[String] = {
    if (isMapKey)
      builder.append('"' + d.toString + '"')
    else
      builder.append(d.toString)
    this
  }

  def addMap(m: Map[AST_PRIMITIVE, AST_PRIMITIVE], isMapKey: Boolean): EmitterState[String] = {
    val b = if (isMapKey) new StringBuilder else builder
    b.append("{")
    var first = true
    m.foreach {
      case (k, v) =>
        if (!first)
          b.append(',')
        first = false
        emit(k, true)
        b.append(':')
        emit(v)
    }
    b.append("}")
    if (isMapKey)
      builder.append(escapeJava(b.result))
    this
  }

  def addNull(isMapKey: Boolean): EmitterState[String] = {
    if (isMapKey)
      builder.append(""""null"""")
    else
      builder.append("null")
    this
  }

  def addString(s: String, isMapKey: Boolean): EmitterState[String] = {
    builder.append(escapeJava(s))
    this
  }

  def result: String = builder.result

}
