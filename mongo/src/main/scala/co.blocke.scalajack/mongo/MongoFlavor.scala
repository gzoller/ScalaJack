package co.blocke.scalajack
package mongo

import model._
import co.blocke.scalajack.typeadapter.CanBuildFromTypeAdapterFactory
import typeadapter.CaseClassTypeAdapterFactory

import java.util.ArrayList
import java.lang.{ UnsupportedOperationException => UOE }

import org.mongodb.scala.bson._
import util.Path

object MongoFlavor extends FlavorMaker {
  type WIRE = BsonValue
  def make(): JackFlavor[BsonValue] = new MongoFlavor()
}

case class MongoFlavor(
    override val defaultHint:        String                       = "_hint",
    override val permissivesOk:      Boolean                      = false,
    override val customAdapters:     List[TypeAdapterFactory]     = List.empty[TypeAdapterFactory],
    override val hintMap:            Map[Type, String]            = Map.empty[Type, String],
    override val hintValueModifiers: Map[Type, HintValueModifier] = Map.empty[Type, HintValueModifier],
    override val typeValueModifier:  Option[HintValueModifier]    = None,
    override val parseOrElseMap:     Map[Type, Type]              = Map.empty[Type, Type],
    override val enumsAsInt:         Boolean                      = false,
    delimiter:                       Char                         = ',') extends JackFlavor[BsonValue] {

  override val stringifyMapKeys: Boolean = true

  final val ID_FIELD = "_id"

  def withAdapters(ta: TypeAdapterFactory*): JackFlavor[BsonValue] = throw new UOE("Not available for MongoDB encoding")
  def withDefaultHint(hint: String): JackFlavor[BsonValue] = throw new UOE("Not available for MongoDB encoding")
  def withHints(h: (Type, String)*): JackFlavor[BsonValue] = throw new UOE("Not available for MongoDB encoding")
  def withHintModifiers(hm: (Type, HintValueModifier)*): JackFlavor[BsonValue] = throw new UOE("Not available for MongoDB encoding")
  def withTypeValueModifier(tm: HintValueModifier): JackFlavor[BsonValue] = throw new UOE("Not available for MongoDB encoding")
  def parseOrElse(poe: (Type, Type)*): JackFlavor[BsonValue] = this.copy(parseOrElseMap = this.parseOrElseMap ++ poe)
  def allowPermissivePrimitives(): JackFlavor[BsonValue] = throw new UOE("Not available for MongoDB encoding")
  def enumsAsInts(): JackFlavor[BsonValue] = this.copy(enumsAsInt = true)

  def stringWrapTypeAdapterFactory[T](wrappedTypeAdapter: TypeAdapter[T]): TypeAdapter[T] = new StringWrapTypeAdapter(wrappedTypeAdapter)

  protected override def bakeContext(): Context =
    new Context(
      Seq(
        CanBuildFromTypeAdapterFactory(this, enumsAsInt, true),
        ObjectIdTypeAdapterFactory,
        ZonedDateTimeTypeAdapterFactory,
        OffsetDateTimeTypeAdapterFactory
      ) ++: super.bakeContext().factories)

  private val writer = MongoWriter(this)

  def parse(wire: BsonValue): Reader[BsonValue] = MongoReader(this, wire, BsonTokenizer().tokenize(wire).asInstanceOf[ArrayList[BsonToken]])

  override def read[T](wire: BsonValue)(implicit tt: TypeTag[T]): T = {
    val p = parse(wire)
    context.typeAdapter(tt.tpe).read(Path.Root, p).asInstanceOf[T]
  }

  def render[T](t: T)(implicit tt: TypeTag[T]): BsonValue = {
    val typeAdapter = context.typeAdapter(tt.tpe).asInstanceOf[TypeAdapter[T]]
    val builder = BsonBuilder()
    typeAdapter.write(t, writer, builder, false)
    builder.result()
  }
}

