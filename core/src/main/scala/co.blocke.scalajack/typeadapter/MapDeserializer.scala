package co.blocke.scalajack
package typeadapter

import scala.collection.GenMapLike

class MapDeserializer[K, V, M <: GenMapLike[K, V, M]] extends Deserializer[M] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[M] = ???

}
