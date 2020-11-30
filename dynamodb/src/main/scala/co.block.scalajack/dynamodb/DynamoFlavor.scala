package co.blocke.scalajack
package dynamodb

import model._
import typeadapter._
import co.blocke.scala_reflection.RType

import scala.jdk.CollectionConverters._
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
  override val defaultHint:        String                           = "_hint",
  override val permissivesOk:      Boolean                          = false,
  override val customAdapters:     List[TypeAdapterFactory]         = List.empty[TypeAdapterFactory],
  override val hintMap:            Map[String, String]              = Map.empty[String, String],
  override val hintValueModifiers: Map[String, HintValueModifier]   = Map.empty[String, HintValueModifier],
  override val typeValueModifier:  HintValueModifier                = DefaultHintModifier,
  override val parseOrElseMap:     Map[Class[_], RType]             = Map.empty[Class[_], RType],
  override val enumsAsInt:         Boolean                          = false
) extends JackFlavor[Item] {

  // $COVERAGE-OFF$Not testing any of this stuff.  Either unused for Dynamo or an exact copy of thoroughly-tested JSON flavor
  override val stringifyMapKeys: Boolean = true

  def stringWrapTypeAdapterFactory[T](
      wrappedTypeAdapter: TypeAdapter[T],
      emptyStringOk:      Boolean        = true
  ): TypeAdapter[T] = ???

  def maybeStringWrapTypeAdapterFactory[T](
    wrappedTypeAdapter: TypeAdapter[T],
    emptyStringOk: Boolean = true
  ): TypeAdapter[T] = ???

  def allowPermissivePrimitives(): JackFlavor[Item] =
    this.copy(permissivesOk = true)
  def enumsAsInts(): JackFlavor[Item] = this.copy(enumsAsInt = true)
  def parseOrElse(poe: (RType, RType)*): JackFlavor[Item] =
    this.copy(parseOrElseMap = this.parseOrElseMap ++ poe.map{(p,oe) => p.infoClass->oe})
  def withAdapters(ta: TypeAdapterFactory*): JackFlavor[Item] =
    this.copy(customAdapters = this.customAdapters ++ ta.toList)
  def withDefaultHint(hint: String): JackFlavor[Item] =
    this.copy(defaultHint = hint)
  def withHints(h: (RType, String)*): JackFlavor[Item] =
    this.copy(hintMap = this.hintMap ++ h.map{(rt,hint) => rt.name->hint})
  def withHintModifiers(hm: (RType, HintValueModifier)*): JackFlavor[Item] =
    this.copy(hintValueModifiers = this.hintValueModifiers ++ hm.map{(rt,hintM) => rt.name->hintM})
  def withTypeValueModifier(tm: HintValueModifier): JackFlavor[Item] =
    this.copy(typeValueModifier = tm)

  def parse(input: Item): Parser = ???
  // $COVERAGE-ON$

  // Embedded JSON-flavored ScalaJack, as Item can read/write JSON, so this is actually the most straightforward
  // path to serialization.
  lazy val sj: JackFlavor[json.JSON] = {
    val baseSj = ScalaJack()
      .withAdapters(customAdapters: _*)
      .withHints(hintMap.map{ case (k,v) => (RType.of(Class.forName(k)),v) }.toList: _*)
      .withHintModifiers(hintValueModifiers.map{ case (k,v) => (RType.of(Class.forName(k)),v)}.toList: _*)
      .withDefaultHint(defaultHint)
      .parseOrElse(parseOrElseMap.map{ case(k,v) => (RType.of(k),v)}.toList: _*)
    baseSj.withTypeValueModifier(typeValueModifier)
  }

  private val jsonWriter = json.JsonWriter() 

  def _read[T](input: Item, typeAdapter: TypeAdapter[T]): T =
    // sj.read[T](input.toJSON.asInstanceOf[json.JSON])
    val parser = json.JsonParser(input.toJSON.asInstanceOf[json.JSON], sj)
    typeAdapter.read(parser).asInstanceOf[T]

  def _render[T](t: T, typeAdapter: TypeAdapter[T]): Item =
    val sb = StringBuilder[json.JSON]()
    typeAdapter.write(t, jsonWriter, sb)
    Item.fromJSON(sb.result().asInstanceOf[String])
    // Item.fromJSON(sj.render[T](t).asInstanceOf[String])
    /*
    def write[WIRE](
      t:      T,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit
    */

  // This is Dynamo-Only.  User will have to case ScalaJack to DynamoFlavor to call this.
  // Yeah, this is a little large-ish for an inlined call, but it *MUST* be inlined for [T]
  // to be known/visible, otherwise it will be (incorrectly) interpreted as Any!
  inline def createTableRequest[T](provisionedThroughput: ProvisionedThroughput): CreateTableRequest = {
    val (optionalTableName, keys, className) = taCache.typeAdapterOf[T] match {
      case ta: classes.ClassTypeAdapterBase[_] => (ta.dbCollectionName, ta.dbKeys, ta.info.name)
    }
    val tableName = optionalTableName.getOrElse(
      throw new java.lang.IllegalStateException(
        s"Class ${className} must be annotated with @Collection to specify a table name."
      )
    )

    val cleanKeys = keys.map(_.asInstanceOf[ClassFieldMember[_, _]])

    if (cleanKeys.isEmpty)
      throw new java.lang.IllegalStateException(
        s"Class ${className} must define at least a primary key with @DBKey."
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
    if (key.valueTypeAdapter.isStringish)
      ScalarAttributeType.S
    else
      ScalarAttributeType.N
}