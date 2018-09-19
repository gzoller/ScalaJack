package co.blocke.scalajack
package json

import org.json4s.JsonAST.JValue

import scala.collection.mutable
import scala.reflect.runtime.universe.{ Type, TypeTag }
import scala.reflect.runtime.currentMirror
import scala.util.control.NonFatal

case class JsonFlavor(
    customAdapters: List[TypeAdapterFactory] = List.empty[TypeAdapterFactory],
    hintMap:        Map[Type, String]        = Map.empty[Type, String],
    hintModifiers:  Map[Type, HintModifier]  = Map.empty[Type, HintModifier],
    typeModifier:   Option[HintModifier]     = None,
    parseOrElseMap: Map[Type, Type]          = Map.empty[Type, Type],
    defaultHint:    String                   = "_hint",
    isCanonical:    Boolean                  = true) extends ScalaJackLike[String] {

  def withAdapters(ta: TypeAdapterFactory*) = this.copy(customAdapters = this.customAdapters ++ ta.toList)
  def withHints(h: (Type, String)*) = this.copy(hintMap = this.hintMap ++ h)
  def withHintModifiers(hm: (Type, HintModifier)*) = this.copy(hintModifiers = this.hintModifiers ++ hm)
  def withDefaultHint(hint: String) = this.copy(defaultHint = hint)
  def withTypeModifier(tm: HintModifier) = this.copy(typeModifier = Some(tm))
  def parseOrElse(poe: (Type, Type)*) = this.copy(parseOrElseMap = this.parseOrElseMap ++ poe)
  def isCanonical(canonical: Boolean) = this.copy(isCanonical = canonical)

  override val context: Context = bakeContext()
  // {
  //   val ctx = bakeContext()
  //   if (isCanonical)
  //     ctx.copy(factories = /*JsonCanBuildFromTypeAdapter :: */ ctx.factories)
  //   else
  //     ctx
  // }

  def read[T](json: String)(implicit valueTypeTag: TypeTag[T]): T = {
    val tokenizer = new Tokenizer(isCanonical)
    val source = json.toCharArray
    val reader = tokenizer.tokenize(source, 0, source.length)
    val Some(js) = JsonParser.parse(json)(Json4sOps)

    val deserializer = context.typeAdapterOf[T].deserializer
    val deserializationResult = deserializer.deserialize(Path.Root, js)(Json4sOps)
    deserializationResult match {
      case DeserializationSuccess(TypeTagged(result)) =>
        result

      case deserializationFailure @ DeserializationFailure(errors) =>
        for (error <- errors) {
          println(error)
          error match {
            case (path, DeserializationError.ExceptionThrown(e)) =>
              println(path)
              e.printStackTrace()

            case _ =>
          }
        }

        throw new DeserializationException(deserializationFailure)
    }
  }

  def render[T](value: T)(implicit valueTypeTag: TypeTag[T]): String = {
    val typeAdapter = context.typeAdapterOf[T]
    implicit val ops: JsonOps[JValue] = Json4sOps
    val serializer = typeAdapter.serializer
    val SerializationSuccess(json) = serializer.serialize[JValue](TypeTagged(value, valueTypeTag.tpe))
    Json4sOps.renderCompact(json, this)
  }
}
