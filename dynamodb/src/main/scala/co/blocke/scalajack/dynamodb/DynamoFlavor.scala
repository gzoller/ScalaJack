package co.blocke.scalajack
package dynamodb

import model._
import ClassHelper.ClassFieldMember
import typeadapter._

import scala.reflect.runtime.universe._
import scala.collection.JavaConverters._
import com.amazonaws.services.dynamodbv2.document.Item
import com.amazonaws.services.dynamodbv2.model.{
  AttributeDefinition,
  CreateTableRequest,
  KeySchemaElement,
  KeyType,
  ProvisionedThroughput,
  ScalarAttributeType
}

case class DynamoFlavor(
    override val defaultHint:        String                       = "_hint",
    override val permissivesOk:      Boolean                      = false,
    override val customAdapters:     List[TypeAdapterFactory]     = List.empty[TypeAdapterFactory],
    override val hintMap:            Map[Type, String]            = Map.empty[Type, String],
    override val hintValueModifiers: Map[Type, HintValueModifier] = Map.empty[Type, HintValueModifier],
    override val typeValueModifier:  HintValueModifier            = DefaultHintModifier,
    override val parseOrElseMap:     Map[Type, Type]              = Map.empty[Type, Type],
    override val enumsAsInt:         Boolean                      = false
) extends JackFlavor[Item] {

  // $COVERAGE-OFF$Not testing any of this stuff.  Either unused for Dynamo or an exact copy of thoroughly-tested JSON flavor
  override val stringifyMapKeys: Boolean = true

  def stringWrapTypeAdapterFactory[T](
      wrappedTypeAdapter: TypeAdapter[T],
      emptyStringOk:      Boolean        = true
  )(implicit tt: TypeTag[T]): TypeAdapter[T] = ???

  def forType[U](implicit tu: TypeTag[U]): JackFlavorFor[Item, U] = ???

  def withAdapters(ta: TypeAdapterFactory*): JackFlavor[Item] =
    this.copy(customAdapters = this.customAdapters ++ ta.toList)
  def withDefaultHint(hint: String): JackFlavor[Item] =
    this.copy(defaultHint = hint)
  def withHints(h: (Type, String)*): JackFlavor[Item] =
    this.copy(hintMap = this.hintMap ++ h)
  def withHintModifiers(hm: (Type, HintValueModifier)*): JackFlavor[Item] =
    this.copy(hintValueModifiers = this.hintValueModifiers ++ hm)
  def withTypeValueModifier(tm: HintValueModifier): JackFlavor[Item] =
    this.copy(typeValueModifier = tm)
  def parseOrElse(poe: (Type, Type)*): JackFlavor[Item] =
    this.copy(parseOrElseMap = this.parseOrElseMap ++ poe)
  def allowPermissivePrimitives(): JackFlavor[Item] =
    this.copy(permissivesOk = true)
  def enumsAsInts(): JackFlavor[Item] = this.copy(enumsAsInt = true)

  def parse(input: Item): Parser = ???
  // $COVERAGE-ON$

  // Embedded JSON-flavored ScalaJack, as Item can read/write JSON, so this is actually the most straightforward
  // path to serialization.
  lazy val sj: JackFlavor[String] = {
    val baseSj = ScalaJack()
      .withAdapters(customAdapters: _*)
      .withHints(hintMap.toList: _*)
      .withHintModifiers(hintValueModifiers.toList: _*)
      .withDefaultHint(defaultHint)
      .parseOrElse(parseOrElseMap.toList: _*)
    baseSj.withTypeValueModifier(typeValueModifier)
  }

  override def read[T](item: Item)(implicit valueTypeTag: TypeTag[T]): T = {
    sj.read[T](item.toJSON)
  }

  def render[T](value: T)(implicit valueTypeTag: TypeTag[T]): Item = {
    Item.fromJSON(sj.render(value))
  }

  // This is Dynamo-Only.  User will have to case ScalaJack to DynamoFlavor to call this.
  def createTableRequest[T](
      provisionedThroughput: ProvisionedThroughput
  )(implicit tt: TypeTag[T]): CreateTableRequest = {
    val tpe = tt.tpe

    val (optionalTableName, keys) = taCache.typeAdapter(tpe.dealias) match {
      case ta: ClassTypeAdapterBase[_] => (ta.dbCollectionName, ta.dbKeys)
    }
    val tableName = optionalTableName.getOrElse(
      throw new java.lang.IllegalStateException(
        s"Class ${tpe.typeSymbol.fullName} must be annotated with @Collection to specify a table name."
      )
    )

    val cleanKeys = keys.map(_.asInstanceOf[ClassFieldMember[_, _]])

    if (cleanKeys.isEmpty)
      throw new java.lang.IllegalStateException(
        s"Class ${tpe.typeSymbol.fullName} must define at least a primary key with @DBKey."
      )

    val attrDetail = cleanKeys.zipWithIndex.collect {
      case (key, idx) if idx == 0 =>
        (
          new AttributeDefinition(key.name, getAttrType(key)),
          new KeySchemaElement(key.name, KeyType.HASH)
        )
      case (key, idx) if idx == 1 =>
        (
          new AttributeDefinition(key.name, getAttrType(key)),
          new KeySchemaElement(key.name, KeyType.RANGE)
        )
    }

    new CreateTableRequest(
      attrDetail.map(_._1).asJava,
      tableName,
      attrDetail.map(_._2).asJava,
      provisionedThroughput
    )
  }

  private def getAttrType(key: ClassFieldMember[_, _]) =
    if (key.valueTypeAdapter.isInstanceOf[Stringish])
      ScalarAttributeType.S
    else
      ScalarAttributeType.N
}
