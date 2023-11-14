package co.blocke.scalajack
package json2

import json.JsonParseError
import scala.annotation._

trait ClassDecoder[A] extends JsonDecoder[A]:
  self =>
  def unsafeDecodeField(in: JsonReader): A

object ClassDecoder:
  def apply[A: scala.reflect.ClassTag](fields: Array[String], fieldDecoders: Array[JsonDecoder[_]]) = new JsonDecoder[A] {
      val fieldMatrix = new StringMatrix(fields)
      // not ideal--use Scala macros, but for now...
      val constructor = summon[scala.reflect.ClassTag[A]].runtimeClass.getConstructors().head

      def unsafeDecode(in: JsonReader): A =
        val fieldValues = new Array[Any](fields.length)
        JsonParser.charWithWS(in, '{')
        if JsonParser.firstField(in) then
          var done = false
          while !done do
            val fieldIdx = JsonParser.parseField(in, fieldMatrix)
            if fieldIdx < 0 then JsonParser.skipValue(in)
            else
              val dec = fieldDecoders(fieldIdx)
              fieldValues(fieldIdx) = dec.unsafeDecode(in)
              if !JsonParser.nextField(in) then done = true
        else throw new JsonParseError("Expected fields!")
        constructor.newInstance(fieldValues*).asInstanceOf[A]
  }