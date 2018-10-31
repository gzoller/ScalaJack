package co.blocke.scalajack
package mongo
package typeadapter

import java.time.OffsetDateTime

import scala.reflect.runtime.universe.TypeTag

object MongoOffsetDateTimeTypeAdapter extends TypeAdapterFactory.=:=[OffsetDateTime] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[OffsetDateTime]): TypeAdapter[OffsetDateTime] = {
    val now = OffsetDateTime.now() // get a Temporal in default time zone
    MongoOffsetDateTimeTypeAdapter(now)
  }

}

case class MongoOffsetDateTimeTypeAdapter(now: OffsetDateTime) extends TypeAdapter[OffsetDateTime] {
  override val serializer: Serializer[OffsetDateTime] = new MongoOffsetDateTimeSerializer()
  override val deserializer: Deserializer[OffsetDateTime] = new MongoOffsetDateTimeDeserializer(now)
}
