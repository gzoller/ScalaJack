package co.blocke.scalajack
package json

import org.json4s.JsonAST._
import model._

//import scala.reflect.runtime.universe.Type

case class JsonFlavor[N](
    //   customAdapters:    List[TypeAdapterFactory] = List.empty[TypeAdapterFactory],
    //   hintMap:           Map[Type, String]        = Map.empty[Type, String],
    //   hintModifiers:     Map[Type, HintModifier]  = Map.empty[Type, HintModifier],
    //   typeModifier:      Option[HintModifier]     = None,
    //   parseOrElseMap:    Map[Type, Type]          = Map.empty[Type, Type],
    defaultHint: String = "_hint"
//   isCanonical:       Boolean                  = true,
//   secondLookParsing: Boolean                  = false
)(implicit tt: TypeTag[N]) extends ScalaJackLike[N] {

  type AST = JValue
  type WIRE = String
  type PARSER_STATE = JsonParserState

  val nativeTypeAdapter: TypeAdapter[N] = context.typeAdapterOf[N]
  def forType[N2](implicit tt: TypeTag[N2]): ScalaJackLike[N2] = JsonFlavor[N2](defaultHint)

  protected def genEmitterState(): EmitterState[String] = JsonEmitterState()
  protected def genParserState(input: WIRE): PARSER_STATE = JsonParserState(input)

  def getArrayParser[E]()(implicit tt: TypeTag[E]): ArrayParser[E] = JsonArrayParser(context.typeAdapterOf[E])
  def getBooleanParser(): Parser = JsonBooleanParser()
  def getIntParser(): Parser = JsonIntParser()

  def toPrimitives(ast: JValue): AST_PRIMITIVE = ast.values

  def fromPrimitives(prim: AST_PRIMITIVE): JValue = prim match {
    case a: List[AST_PRIMITIVE] => JArray(a.map(e => fromPrimitives(e)))
    case b: Boolean             => JBool(b)
    case b: BigInt              => JInt(b)
    case d: BigDecimal          => JDecimal(d)
    case m: Map[AST_PRIMITIVE, AST_PRIMITIVE] =>
      JObject(m.map {
        case (k, v) =>
          (fromPrimitives(k) match {
            case JString(s) => s
            case x          => emit(x)
          }, fromPrimitives(v))
      }.toSeq: _*)
    case null      => JNull
    case s: String => JString(s)
  }

  /*
  def withAdapters(ta: TypeAdapterFactory*) = this.copy(customAdapters = this.customAdapters ++ ta.toList)
  def withHints(h: (Type, String)*) = this.copy(hintMap = this.hintMap ++ h)
  def withHintModifiers(hm: (Type, HintModifier)*) = this.copy(hintModifiers = this.hintModifiers ++ hm)
  def withDefaultHint(hint: String) = this.copy(defaultHint = hint)
  def withTypeModifier(tm: HintModifier) = this.copy(typeModifier = Some(tm))
  def parseOrElse(poe: (Type, Type)*) = this.copy(parseOrElseMap = this.parseOrElseMap ++ poe)
  def isCanonical(canonical: Boolean) = this.copy(isCanonical = canonical)
  def withSecondLookParsing() = this.copy(secondLookParsing = true)

  implicit val guidance: SerializationGuidance = {
    val first = if (secondLookParsing)
      SerializationGuidance(secondLookParsing = true)
    else
      SerializationGuidance()
    if (isCanonical)
      first.copy(isCanonical = true)
    else
      first
  }

  implicit val ops: Ops[JValue, String] = Json4sOps

  def render[T](value: T)(implicit valueTypeTag: TypeTag[T]): String = {
    val typeAdapter = context.typeAdapterOf[T]
    typeAdapter.irTransceiver.write(TypeTagged(value, valueTypeTag.tpe)) match {
      case WriteSuccess(json)                              => Json4sOps.serialize(json, this)
      case WriteFailure(f) if f == Seq(WriteError.Nothing) => ""
    }
  }
  */
}