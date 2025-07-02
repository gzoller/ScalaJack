package co.blocke.scalajack

import json.JsonCodec
import json.writing.JsonOutput
import json.reading.JsonSource

object Raw:
  opaque type JsonRaw = String

  given JsonDefault[JsonRaw] with
    def default: JsonRaw = Raw("")

  given JsonCodec[JsonRaw] with
    def encodeValue(in: JsonRaw, out: JsonOutput): Unit =
      out.rawJsonValue(Raw.unwrap(in)) // emit underlying string

    def decodeValue(in: JsonSource): JsonRaw =
      Raw(in.readRawJson()) // parse string and wrap

  def apply(value: String): JsonRaw = value
  def unwrap(raw: JsonRaw): String = raw