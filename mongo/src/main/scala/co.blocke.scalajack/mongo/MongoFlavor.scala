package co.blocke.scalajack
package mongo

import org.bson.BsonValue
import typeadapter._

import scala.reflect.runtime.universe.{ TypeTag, Type }

case class MongoFlavor(
    customAdapters: List[TypeAdapterFactory] = List.empty[TypeAdapterFactory],
    hintMap:        Map[Type, String]        = Map.empty[Type, String],
    hintModifiers:  Map[Type, HintModifier]  = Map.empty[Type, HintModifier],
    typeModifier:   Option[HintModifier]     = None,
    parseOrElseMap: Map[Type, Type]          = Map.empty[Type, Type],
    defaultHint:    String                   = "_hint",
    isCanonical:    Boolean                  = true
) extends ScalaJackLike[BsonValue] with JackFlavor[BsonValue] {

  val bsonParser = new BsonParser

  def withAdapters(ta: TypeAdapterFactory*) = this.copy(customAdapters = this.customAdapters ++ ta.toList)
  def withHints(h: (Type, String)*) = this.copy(hintMap = this.hintMap ++ h)
  def withHintModifiers(hm: (Type, HintModifier)*) = this.copy(hintModifiers = this.hintModifiers ++ hm)
  def withDefaultHint(hint: String) = this.copy(defaultHint = hint)
  def withTypeModifier(tm: HintModifier) = throw new UnsupportedOperationException("Not available for Mongo formatting")
  def parseOrElse(poe: (Type, Type)*) = this.copy(parseOrElseMap = this.parseOrElseMap ++ poe)
  def isCanonical(canonical: Boolean) = throw new UnsupportedOperationException("Not available for Mongo formatting")

  override protected def bakeContext(): Context = {
    val ctx = super.bakeContext()
    ctx.copy(factories = MongoCaseClassTypeAdapter :: MongoOffsetDateTimeTypeAdapter :: MongoZonedDateTimeTypeAdapter :: BsonObjectIdTypeAdapter :: BsonDateTimeTypeAdapter :: ctx.factories)
  }

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
