package co.blocke.scalajackx.hybrid

//case class StringEmitterState() {
//  val builder = new StringBuilder()
//}

trait StringEmitter extends Emitter { // Wire writer

  type WIRE = String
  type EmitterState = StringBuilder

  def emit(prim: Any, es: EmitterState): String =
    (prim match {
      case i: BigInt => es.append(i.toString)
      case list: List[_] =>
        es.append("[")
        var first = true
        list.map { e =>
          if (!first)
            es.append(",")
          first = false
          emit(e, es)
        }
        es.append("]")
      case _ => es
    }).result
}

// These flavors are per-PRIMITIVE type
case class IntJsonSerializer() extends Serializer2 with JsonIntParser with StringEmitter
case class ArrayJsonSerializer[E](elementTypeAdapter: TypeAdapter[E]) extends ArraySerializer[E] with JsonArrayParser[E] with StringEmitter
