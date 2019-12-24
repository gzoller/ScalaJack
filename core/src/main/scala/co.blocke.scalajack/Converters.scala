package co.blocke.scalajack

import co.blocke.scalajack.json.JsonFlavor
import model._
import yaml.YamlFlavor
import delimited.DelimitedFlavor
import json4s.Json4sFlavor
import org.json4s.JValue

import scala.reflect.runtime.universe._

case class Configuration(
    defaultHint: String = "_hint",
    hintMap: Map[Type, String] = Map.empty[Type, String],
    hintValueModifiers: Map[Type, HintValueModifier] = Map.empty[Type, HintValueModifier],
    typeValueModifier: HintValueModifier = DefaultHintModifier,
    enumsAsInt: Boolean = false,
    customAdapters: List[TypeAdapterFactory] = List.empty[TypeAdapterFactory],
    parseOrElseMap: Map[Type, Type] = Map.empty[Type, Type],
    permissivesOk: Boolean = false,
    delimiter: Char = ','
) {
  // $COVERAGE-OFF$All this is carbon-copy from Flavors, so cut some testing noise here
  def withDefaultHint(hint: String): Configuration                     = this.copy(defaultHint = hint)
  def withHints(h: (Type, String)*): Configuration                     = this.copy(hintMap = this.hintMap ++ h)
  def withHintModifiers(hm: (Type, HintValueModifier)*): Configuration = this.copy(hintValueModifiers = this.hintValueModifiers ++ hm)
  def withTypeValueModifier(tm: HintValueModifier): Configuration      = this.copy(typeValueModifier = tm)
  def enumsAsInts(): Configuration                                     = this.copy(enumsAsInt = true)
  def withAdapters(ta: TypeAdapterFactory*): Configuration             = this.copy(customAdapters = this.customAdapters ++ ta.toList)
  def parseOrElse(poe: (Type, Type)*): Configuration                   = this.copy(parseOrElseMap = this.parseOrElseMap ++ poe)
  def allowPermissivePrimitives(): Configuration                       = this.copy(permissivesOk = true)
  def withDelimiter(delim: Char): Configuration                        = this.copy(delimiter = delim)
  // $COVERAGE-ON$
}

object Converters {

  case class SJHolder() {
    lazy val sjJson: JackFlavor[JSON] = JsonFlavor(
      config.defaultHint,
      config.permissivesOk,
      config.customAdapters,
      config.hintMap,
      config.hintValueModifiers,
      config.typeValueModifier,
      config.parseOrElseMap,
      config.enumsAsInt
    )
    lazy val sjYaml: JackFlavor[YAML] = YamlFlavor(
      config.defaultHint,
      config.permissivesOk,
      config.customAdapters,
      config.hintMap,
      config.hintValueModifiers,
      config.typeValueModifier,
      config.parseOrElseMap,
      config.enumsAsInt
    )
    lazy val sjJson4s: JackFlavor[JValue] =
      Json4sFlavor(
        config.defaultHint,
        config.permissivesOk,
        config.customAdapters,
        config.hintMap,
        config.hintValueModifiers,
        config.typeValueModifier,
        config.parseOrElseMap,
        config.enumsAsInt
      )
    lazy val sjDelimited: JackFlavor[DELIMITED] =
      DelimitedFlavor(
        config.delimiter,
        config.defaultHint,
        config.permissivesOk,
        config.customAdapters,
        config.hintMap,
        config.hintValueModifiers,
        config.typeValueModifier,
        config.parseOrElseMap,
        config.enumsAsInt
      )
  }

  private var config = Configuration()
  private var holder = SJHolder()

  def withConfig(cfg: Configuration): Unit = {
    config = cfg
    holder = SJHolder() // reset holder
  }

  implicit class StringishSerializers(s: String) {
    def yamlToJson: JSON                                       = s.mapYamlTo[Any, JSON](holder.sjJson)(a => a)
    def yamlToJson4s: JValue                                   = s.mapYamlTo[Any, JValue](holder.sjJson4s)(a => a)
    def yamlToDelimited[T](implicit tt: TypeTag[T]): DELIMITED = s.mapYamlTo[T, DELIMITED](holder.sjDelimited)(a => a)

    def jsonToYaml: YAML                                       = s.mapJsonTo[Any, YAML](holder.sjYaml)(a => a)
    def jsonToJson4s: JValue                                   = s.mapJsonTo[Any, JValue](holder.sjJson4s)(a => a)
    def jsonToDelimited[T](implicit tt: TypeTag[T]): DELIMITED = s.mapJsonTo[T, DELIMITED](holder.sjDelimited)(a => a)

    def delimitedToYaml[T](implicit tt: TypeTag[T]): YAML     = s.mapDelimitedTo[T, YAML](holder.sjYaml)(a => a)
    def delimitedToJson4s[T](implicit tt: TypeTag[T]): JValue = s.mapDelimitedTo[T, JValue](holder.sjJson4s)(a => a)
    def delimitedToJson[T](implicit tt: TypeTag[T]): JSON     = s.mapDelimitedTo[T, JSON](holder.sjJson)(a => a)

    def mapJsonTo[T, S](toFlavor: JackFlavor[S])(fn: T => T)(implicit tt: TypeTag[T]): S      = { toFlavor.render[T](fn(holder.sjJson.read[T](s))) }
    def mapYamlTo[T, S](toFlavor: JackFlavor[S])(fn: T => T)(implicit tt: TypeTag[T]): S      = { toFlavor.render[T](fn(holder.sjYaml.read[T](s))) }
    def mapDelimitedTo[T, S](toFlavor: JackFlavor[S])(fn: T => T)(implicit tt: TypeTag[T]): S = { toFlavor.render[T](fn(holder.sjDelimited.read[T](s))) }

    def mapJson[T](fn: T => T)(implicit tt: TypeTag[T]): JSON           = holder.sjJson.render[T](fn(holder.sjJson.read[T](s)))
    def mapYaml[T](fn: T => T)(implicit tt: TypeTag[T]): JSON           = holder.sjYaml.render[T](fn(holder.sjYaml.read[T](s)))
    def mapDelimited[T](fn: T => T)(implicit tt: TypeTag[T]): DELIMITED = holder.sjDelimited.render[T](fn(holder.sjDelimited.read[T](s)))
  }

  implicit class JValueSerializers(s: JValue) {
    def json4sToYaml: YAML                                       = s.mapJson4sTo[Any, YAML](holder.sjYaml)(a => a)
    def json4sToJson: JSON                                       = s.mapJson4sTo[Any, JSON](holder.sjJson)(a => a)
    def json4sToDelimited[T](implicit tt: TypeTag[T]): DELIMITED = s.mapJson4sTo[T, DELIMITED](holder.sjDelimited)(a => a)

    def mapJson4s[T](fn: T => T)(implicit tt: TypeTag[T]): JValue                          = holder.sjJson4s.render[T](fn(holder.sjJson4s.read[T](s)))
    def mapJson4sTo[T, S](toFlavor: JackFlavor[S])(fn: T => T)(implicit tt: TypeTag[T]): S = { toFlavor.render[T](fn(holder.sjJson4s.read[T](s))) }
  }

  implicit class AnyConvenience(a: Any) {
    def toJson[T](implicit tt: TypeTag[T]): JSON = holder.sjJson.render(a.asInstanceOf[T])
    def fromJson[T](implicit tt: TypeTag[T]): T  = holder.sjJson.read[T](a.asInstanceOf[JSON])

    def toJson4s[T](implicit tt: TypeTag[T]): JValue = holder.sjJson4s.render(a.asInstanceOf[T])
    def fromJson4s[T](implicit tt: TypeTag[T]): T    = holder.sjJson4s.read[T](a.asInstanceOf[JValue])

    def toYaml[T](implicit tt: TypeTag[T]): YAML = holder.sjYaml.render(a.asInstanceOf[T])
    def fromYaml[T](implicit tt: TypeTag[T]): T  = holder.sjYaml.read[T](a.asInstanceOf[YAML])

    def toDelimited[T](implicit tt: TypeTag[T]): DELIMITED = holder.sjDelimited.render(a.asInstanceOf[T])
    def fromDelimited[T](implicit tt: TypeTag[T]): T       = holder.sjDelimited.read[T](a.asInstanceOf[DELIMITED])
  }
}
