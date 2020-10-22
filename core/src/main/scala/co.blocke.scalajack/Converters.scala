package co.blocke.scalajack

import co.blocke.scalajack.json.JsonFlavor
import model._
import yaml.YamlFlavor
import delimited.DelimitedFlavor
import json4s.Json4sFlavor
import org.json4s.JValue
import json._
import yaml._
import delimited._

object Converters:

  //------
  // Note: Json, Json4s, and Yaml do NOT have a 'toDelimited' option because delimited format is *ordered*, while 
  //       other 3 are intrinsically un-ordered, so there's no guarantee the delimited fields will be rendered in the
  //       'correct' order.
  //------

  // JSON mappers
  extension (j: JSON)
    def jsonToYaml(implicit sjY: JackFlavor[YAML], sjJ: JackFlavor[JSON]): YAML                = sjY.render( sjJ.read[Any](j) )
    def jsonToJson4s(implicit sjV: JackFlavor[JValue], sjJ: JackFlavor[JSON]): JValue          = sjV.render( sjJ.read[Any](j) )

  extension[T, S] (j: JSON)
    inline def mapJsonTo(toFlavor: JackFlavor[S])(fn: T => T)(implicit sjJ: JackFlavor[JSON]): S = toFlavor.render[T](fn(sjJ.read[T](j)))

  extension[T] (j: JSON)
    inline def fromJson(implicit sjJ: JackFlavor[JSON]): T               = sjJ.read[T](j)
    inline def mapJson(fn: T => T)(implicit sjJ: JackFlavor[JSON]): JSON = sjJ.render[T](fn(sjJ.read[T](j)))


  // YAML mappers
  extension (y: YAML)
    def yamlToJson(implicit sjJ: JackFlavor[JSON], sjY: JackFlavor[YAML]): JSON                = sjJ.render( sjY.read[Any](y) )
    def yamlToJson4s(implicit sjV: JackFlavor[JValue], sjY: JackFlavor[YAML]): JValue          = sjV.render( sjY.read[Any](y) )

  extension[T, S] (y: YAML)
    inline def mapYamlTo(toFlavor: JackFlavor[S])(fn: T => T)(implicit sjY: JackFlavor[YAML]): S = toFlavor.render[T](fn(sjY.read[T](y)))

  extension[T] (y: YAML)
    inline def fromYaml(implicit sjY: JackFlavor[YAML]): T               = sjY.read[T](y)
    inline def mapYaml(fn: T => T)(implicit sjY: JackFlavor[YAML]): YAML = sjY.render[T](fn(sjY.read[T](y)))

    
  // DELIMITED mappers
  extension[T, S] (d: DELIMITED)
    inline def mapDelimitedTo(toFlavor: JackFlavor[S])(fn: T => T)(implicit sjD: JackFlavor[DELIMITED]): S = toFlavor.render[T](fn(sjD.read[T](d)))

  // Delimited needs T here because there are no key (field name) information... only the values.
  extension[T] (d: DELIMITED)
    inline def delimitedToJson(implicit sjJ: JackFlavor[JSON], sjD: JackFlavor[DELIMITED]): JSON       = sjJ.render( sjD.read[T](d) )
    inline def delimitedToJson4s(implicit sjV: JackFlavor[JValue], sjD: JackFlavor[DELIMITED]): JValue = sjV.render( sjD.read[T](d) )
    inline def delimitedToYaml(implicit sjY: JackFlavor[YAML], sjD: JackFlavor[DELIMITED]): YAML       = sjY.render( sjD.read[T](d) )
    inline def fromDelimited(implicit sjD: JackFlavor[DELIMITED]): T                                   = sjD.read[T](d)
    inline def mapDelimited(fn: T => T)(implicit sjD: JackFlavor[DELIMITED]): DELIMITED                = sjD.render[T](fn(sjD.read[T](d)))


  // Json4s mappers
  extension (j: JValue)
    def json4sToYaml(implicit sjY: JackFlavor[YAML], sjV: JackFlavor[JValue]): YAML                = sjY.render( sjV.read[Any](j) )
    def json4sToJson(implicit sjJ: JackFlavor[JSON], sjV: JackFlavor[JValue]): JSON                = sjJ.render( sjV.read[Any](j) )

  extension[T, S] (j: JValue)
    inline def mapJson4sTo(toFlavor: JackFlavor[S])(fn: T => T)(implicit sjV: JackFlavor[JValue]): S = toFlavor.render[T](fn(sjV.read[T](j)))

  extension[T] (j: JValue)
    inline def fromJson4s(implicit sjV: JackFlavor[JValue]): T                 = sjV.read[T](j)
    inline def mapJson4s(fn: T => T)(implicit sjV: JackFlavor[JValue]): JValue = sjV.render[T](fn(sjV.read[T](j)))

  
  extension[T] (a: T)
    inline def toJson(implicit sjJ: JackFlavor[JSON]): JSON                = sjJ.render[T](a)
    inline def toYaml(implicit sjY: JackFlavor[YAML]): YAML                = sjY.render[T](a)
    inline def toDelimited(implicit sjD: JackFlavor[DELIMITED]): DELIMITED = sjD.render[T](a)
    inline def toJson4s(implicit sjV: JackFlavor[JValue]): JValue          = sjV.render[T](a)
