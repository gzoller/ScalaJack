package co.blocke.scalajack
package mongo

import org.bson._
import model.JackFlavor
import co.blocke.scalajack.Converters._
import co.blocke.scalajack.json._
import json4s._
import yaml._ 
import delimited._
import org.json4s.JValue

object Converters:

  extension[T, S] (b: BsonValue)
    inline def mapMongoTo(toFlavor: JackFlavor[S])(fn: T => T)(implicit sjB: JackFlavor[BsonValue]): S = toFlavor.render[T](fn(sjB.read[T](b)))
  
  // montoTo... flavors need T to be able to handle _id (DBKey) fields.
  extension[T] (b: BsonValue)
    inline def mongoToJson(implicit sjJ: JackFlavor[JSON], sjB: JackFlavor[BsonValue]): JSON          = sjJ.render( sjB.read[T](b) )
    inline def mongoToYaml(implicit sjY: JackFlavor[YAML], sjB: JackFlavor[BsonValue]): YAML          = sjY.render( sjB.read[T](b) )
    inline def mongoToJson4s(implicit sjV: JackFlavor[JValue], sjB: JackFlavor[BsonValue]): JValue    = sjV.render( sjB.read[T](b) )
    inline def fromMongo(implicit sjB: JackFlavor[BsonValue]): T                                      = sjB.read[T](b)
    inline def mapMongo(fn: T => T)(implicit sjB: JackFlavor[BsonValue]): BsonValue                   = sjB.render[T](fn(sjB.read[T](b)))

  // Tie in other converters...
  extension[T] (j: JSON)
    inline def jsonToMongo(implicit sjB: JackFlavor[BsonValue], sjJ: JackFlavor[JSON]): BsonValue     = sjB.render( sjJ.read[T](j) )

  extension[T] (y: YAML)
    inline def yamlToMongo(implicit sjB: JackFlavor[BsonValue], sjY: JackFlavor[YAML]): BsonValue     = sjB.render( sjY.read[T](y) )

  extension[T] (j: JValue)
    inline def json4sToMongo(implicit sjB: JackFlavor[BsonValue], sjV: JackFlavor[JValue]): BsonValue = sjB.render( sjV.read[T](j) )

  extension[T] (a: T)
    inline def toMongo(implicit sjB: JackFlavor[BsonValue]): BsonValue                                = sjB.render[T](a)