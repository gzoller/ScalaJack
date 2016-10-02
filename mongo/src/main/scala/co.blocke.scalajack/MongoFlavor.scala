package co.blocke.scalajack
package mongo

import org.bson.BsonValue

import scala.reflect.runtime.universe.TypeTag

case class MongoFlavor() extends JackFlavor[BsonValue] with ScalaJackLike[BsonValue] {

  val bsonParser = new BsonParser

  context.withFactory(OffsetDateTimeTypeAdapter)
  context.withFactory(BsonDateTimeTypeAdapter)

  override def read[T](src: BsonValue)(implicit valueTypeTag: TypeTag[T]): T = {
    val bsonReader = bsonParser.parse(src)
    val typeAdapter = context.typeAdapter(valueTypeTag.tpe).asInstanceOf[TypeAdapter[Any]]
    typeAdapter.read(bsonReader).asInstanceOf[T]
  }

  override def render[T](instance: T)(implicit tt: TypeTag[T]): BsonValue = {
    val bsonWriter = new BsonWriter
    val typeAdapter = context.typeAdapter(tt.tpe).asInstanceOf[TypeAdapter[Any]]
    typeAdapter.write(instance, bsonWriter)
    bsonWriter.RootStructure.value
  }

}
