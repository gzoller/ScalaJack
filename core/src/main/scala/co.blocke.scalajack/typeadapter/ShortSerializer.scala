package co.blocke.scalajack
package typeadapter

class ShortSerializer extends Serializer[Short] {

  override def serialize[J](tagged: TypeTagged[Short])(implicit ops: JsonOps[J]): SerializationResult[J] = ???

}
