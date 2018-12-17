package co.blocke.scalajack
package json

import play.api.libs.json._
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
)(implicit tt: TypeTag[N]) extends ScalaJackLike[N, JsValue, String] {

  implicit val ops: Ops[JsValue] = PlayJsonOps

  val nativeTypeAdapter: TypeAdapter[N] = context.typeAdapterOf[N]
  def forType[N2](implicit tt: TypeTag[N2]): ScalaJackLike[N2, JsValue, String] = JsonFlavor[N2](defaultHint)

  def parse(src: String): JsValue = Json.parse(src)
  def emit(ast: JsValue): String = Json.stringify(ast)

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

  def render[T](value: T)(implicit valueTypeTag: TypeTag[T]): String = {
    val typeAdapter = context.typeAdapterOf[T]
    typeAdapter.irTransceiver.write(TypeTagged(value, valueTypeTag.tpe)) match {
      case WriteSuccess(json)                              => Json4sOps.serialize(json, this)
      case WriteFailure(f) if f == Seq(WriteError.Nothing) => ""
    }
  }
  */
}