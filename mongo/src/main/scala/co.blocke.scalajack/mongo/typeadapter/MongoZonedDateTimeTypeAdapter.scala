package co.blocke.scalajack
package mongo
package typeadapter

import java.time._

import scala.reflect.runtime.universe.TypeTag

object MongoZonedDateTimeTypeAdapter extends TypeAdapterFactory.=:=[ZonedDateTime] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[ZonedDateTime]): TypeAdapter[ZonedDateTime] = {
    MongoZonedDateTimeTypeAdapter()
  }

}

case class MongoZonedDateTimeTypeAdapter() extends TypeAdapter[ZonedDateTime] {
  override val serializer: Serializer[ZonedDateTime] = new MongoZonedDateTimeSerializer()
  override val deserializer: Deserializer[ZonedDateTime] = new MongoZonedDateTimeDeserializer()
}
