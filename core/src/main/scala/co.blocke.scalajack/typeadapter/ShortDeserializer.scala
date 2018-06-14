package co.blocke.scalajack
package typeadapter

class ShortDeserializer extends Deserializer[Short] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[Short] = ???

}
