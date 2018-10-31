package co.blocke.scalajack
package mongo

import org.bson.BsonValue
import org.mongodb.scala.bson.collection.immutable.Document
import typeadapter._

import scala.reflect.runtime.universe.{ Type, TypeTag }

case class MongoFlavor(
    customAdapters:    List[TypeAdapterFactory] = List.empty[TypeAdapterFactory],
    hintMap:           Map[Type, String]        = Map.empty[Type, String],
    hintModifiers:     Map[Type, HintModifier]  = Map.empty[Type, HintModifier],
    typeModifier:      Option[HintModifier]     = None,
    parseOrElseMap:    Map[Type, Type]          = Map.empty[Type, Type],
    defaultHint:       String                   = "_hint",
    isCanonical:       Boolean                  = true,
    secondLookParsing: Boolean                  = false) extends ScalaJackLike[BsonValue, Document] with JackFlavor[BsonValue, Document] {

  def withAdapters(ta: TypeAdapterFactory*) = this.copy(customAdapters = this.customAdapters ++ ta.toList)
  def withHints(h: (Type, String)*) = this.copy(hintMap = this.hintMap ++ h)
  def withHintModifiers(hm: (Type, HintModifier)*) = this.copy(hintModifiers = this.hintModifiers ++ hm)
  def withDefaultHint(hint: String) = this.copy(defaultHint = hint)
  def withTypeModifier(tm: HintModifier) = throw new UnsupportedOperationException("Not available for Mongo formatting")
  def parseOrElse(poe: (Type, Type)*) = this.copy(parseOrElseMap = this.parseOrElseMap ++ poe)
  def isCanonical(canonical: Boolean) = throw new UnsupportedOperationException("Not available for Mongo formatting")
  def withSecondLookParsing() = this.copy(secondLookParsing = true)

  implicit val ops = BsonOps
  implicit val guidance: SerializationGuidance = SerializationGuidance()

  def readSafely[T](doc: Document)(implicit tt: TypeTag[T]): Either[DeserializationFailure, T] = {
    val typeAdapter = context.typeAdapterOf[T]
    val deserializer = typeAdapter.deserializer
    val ast = ops.parse(doc)
    deserializer.deserialize[BsonValue, Document](Path.Root, ast) match {
      case DeserializationSuccess(s)    => Right(s.get)
      case fail: DeserializationFailure => Left(fail)
    }
  }

  def render[T](value: T)(implicit valueTypeTag: TypeTag[T]): Document = {
    val typeAdapter = context.typeAdapterOf[T]
    val serializer = typeAdapter.serializer
    serializer.serialize[BsonValue, Document](TypeTagged(value, valueTypeTag.tpe)) match {
      case SerializationSuccess(doc)                                       => ops.renderCompact(doc, this)
      case SerializationFailure(f) if f == Seq(SerializationError.Nothing) => null // throw SerializationException here???
    }
  }

  def parse(doc: Document): BsonValue = ops.parse(doc)

  def emit(ast: BsonValue): Document = ops.renderCompact(ast, this)

  override protected def bakeContext(): Context = {
    val ctx = super.bakeContext()
    ctx.copy(factories = MongoCaseClassTypeAdapter :: BsonDateTimeTypeAdapter :: MongoOffsetDateTimeTypeAdapter :: MongoZonedDateTimeTypeAdapter :: BsonObjectIdTypeAdapter :: ctx.factories)
  }

}
