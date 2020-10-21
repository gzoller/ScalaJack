package co.blocke.scalajack
package mongo

import model._
import typeadapter._
import org.bson._
import co.blocke.scala_reflection.RType

import scala.collection.mutable

final val ID_FIELD = "_id"

case class MongoFlavor(
  override val defaultHint:        String                         = "_hint",
  override val permissivesOk:      Boolean                        = false,
  override val customAdapters:     List[TypeAdapterFactory]       = List.empty[TypeAdapterFactory],
  override val hintMap:            Map[String, String]            = Map.empty[String, String],
  override val hintValueModifiers: Map[String, HintValueModifier] = Map.empty[String, HintValueModifier],
  override val typeValueModifier:  HintValueModifier              = DefaultHintModifier,
  override val parseOrElseMap:     Map[Class[_], RType]           = Map.empty[Class[_], RType],
  override val enumsAsInt:         Boolean                        = false
) extends JackFlavor[BsonValue] {

  override val stringifyMapKeys: Boolean = true

  def allowPermissivePrimitives(): JackFlavor[BsonValue] =
    this.copy(permissivesOk = true)
  def enumsAsInts(): JackFlavor[BsonValue] = this.copy(enumsAsInt = true)
  def parseOrElse(poe: (RType, RType)*): JackFlavor[BsonValue] =
    this.copy(parseOrElseMap = this.parseOrElseMap ++ poe.map{(p,oe) => p.infoClass->oe})
  def withAdapters(ta: TypeAdapterFactory*): JackFlavor[BsonValue] =
    this.copy(customAdapters = this.customAdapters ++ ta.toList)
  def withDefaultHint(hint: String): JackFlavor[BsonValue] =
    this.copy(defaultHint = hint)
  def withHints(h: (RType, String)*): JackFlavor[BsonValue] =
    this.copy(hintMap = this.hintMap ++ h.map{(rt,hint) => rt.name->hint})
  def withHintModifiers(hm: (RType, HintValueModifier)*): JackFlavor[BsonValue] =
    this.copy(hintValueModifiers = this.hintValueModifiers ++ hm.map{(rt,hintM) => rt.name->hintM})
  def withTypeValueModifier(tm: HintValueModifier): JackFlavor[BsonValue] =
    this.copy(typeValueModifier = tm)    

  def stringWrapTypeAdapterFactory[T](
      wrappedTypeAdapter: TypeAdapter[T],
      emptyStringOk:      Boolean        = true
  ): TypeAdapter[T] =
    new StringWrapTypeAdapter(wrappedTypeAdapter)

  def maybeStringWrapTypeAdapterFactory[T](
    wrappedTypeAdapter: TypeAdapter[T],
    emptyStringOk: Boolean = true
  ): TypeAdapter[T] =
    co.blocke.scalajack.typeadapter.MaybeStringWrapTypeAdapter(this, wrappedTypeAdapter, emptyStringOk)

  override def bakeCache(): TypeAdapterCache = {
    val dads = super.bakeCache()
    dads.copy(
      factories = List(
        ObjectIdTypeAdapter,
        OffsetDateTimeTypeAdapter,
        ZonedDateTimeTypeAdapter
      ) ++ dads.factories.toList
    )
  }

  private val writer = MongoWriter(anyTypeAdapter)

  def parse(input: BsonValue): Parser = BsonParser(input, this)

  def _read[T](input: BsonValue, typeAdapter: TypeAdapter[T]): T =
    val parser = BsonParser(input, this)
    typeAdapter.read(parser).asInstanceOf[T]

  def _render[T](t: T, typeAdapter: TypeAdapter[T]): BsonValue =
    val sb = mongo.BsonBuilder()
    typeAdapter.write(t, writer, sb)
    sb.result()

  override def getBuilder: mutable.Builder[BsonValue, BsonValue] = mongo.BsonBuilder()
}