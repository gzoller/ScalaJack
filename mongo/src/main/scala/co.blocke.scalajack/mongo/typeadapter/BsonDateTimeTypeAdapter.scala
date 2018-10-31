package co.blocke.scalajack
package mongo
package typeadapter

/*
 This is a raw BsonDateTime type adapter.  There are just so many variants of Java and Joda dates and times it is
 futile to attempt to support every need.  OffsetDateTime and ZonedDateTime are given for reference and general use.
 This raw BsonDateTime allows you to handle the raw value any way you wish... or (even better) don't use this type
 adapter and write your own specific type adapter like we did for OffsetDateTime and ZonedDateTime.
 */

object BsonDateTimeTypeAdapter extends TypeAdapterFactory.===[MongoTime] {

  override def create(next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[MongoTime]): TypeAdapter[MongoTime] = {
    BsonDateTimeTypeAdapter()
  }

}

case class BsonDateTimeTypeAdapter() extends TypeAdapter[MongoTime] {
  override val serializer: Serializer[MongoTime] = new BsonDateTimeSerializer()
  override val deserializer: Deserializer[MongoTime] = new BsonDateTimeDeserializer()
}

