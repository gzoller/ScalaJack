package co.blocke.scalajack
package json

import org.json4s.JsonAST.{ JNull, JValue }
import scala.reflect.runtime.universe.{ Type, TypeTag }

case class JsonFlavor(
    customAdapters:    List[TypeAdapterFactory] = List.empty[TypeAdapterFactory],
    hintMap:           Map[Type, String]        = Map.empty[Type, String],
    hintModifiers:     Map[Type, HintModifier]  = Map.empty[Type, HintModifier],
    typeModifier:      Option[HintModifier]     = None,
    parseOrElseMap:    Map[Type, Type]          = Map.empty[Type, Type],
    defaultHint:       String                   = "_hint",
    isCanonical:       Boolean                  = true,
    secondLookParsing: Boolean                  = false) extends ScalaJackLike[String, JValue] {

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

  def read[T](json: String)(implicit valueTypeTag: TypeTag[T]): T = {
    val deserializationResult = try {
      val Some(js) = JsonParser.parse(json)(Json4sOps)
      val deserializer = context.typeAdapterOf[T].deserializer
      deserializer.deserialize(Path.Root, js)(Json4sOps, guidance)
    } catch {
      case e: Exception => DeserializationFailure(Path.Unknown, DeserializationError.ExceptionThrown(e))
    }
    deserializationResult match {
      case DeserializationSuccess(TypeTagged(result)) =>
        result

      case deserializationFailure @ DeserializationFailure(errors) =>
        /*  What's this all for?
        for (error <- errors) {
          //          println(error)
          error match {
            case (path, DeserializationError.ExceptionThrown(e)) =>
              //              println(path)
              e.printStackTrace()

            case _ =>
          }
        }
        */

        throw new DeserializationException(deserializationFailure)
    }
  }

  def parse(json: String): JValue =
    JsonParser.parse(json)(Json4sOps).getOrElse(JNull)

  def render[T](value: T)(implicit valueTypeTag: TypeTag[T]): String = {
    val typeAdapter = context.typeAdapterOf[T]
    implicit val ops: JsonOps[JValue] = Json4sOps
    val serializer = typeAdapter.serializer
    serializer.serialize[JValue](TypeTagged(value, valueTypeTag.tpe))(Json4sOps, guidance) match {
      case SerializationSuccess(json)                                      => Json4sOps.renderCompact(json, this)
      case SerializationFailure(f) if f == Seq(SerializationError.Nothing) => ""
    }
  }

  def render(ast: JValue): String =
    Json4sOps.renderCompact(ast, this)

}
