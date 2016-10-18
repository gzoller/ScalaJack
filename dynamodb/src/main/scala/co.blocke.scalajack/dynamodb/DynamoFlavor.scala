package co.blocke.scalajack
package dynamodb

import scala.collection.mutable
import scala.reflect.runtime.universe.{ Type, TypeTag }
import scala.reflect.runtime.currentMirror
import scala.collection.JavaConverters._

import typeadapter.CaseClassTypeAdapter
import CaseClassTypeAdapter.Member

import com.amazonaws.services.dynamodbv2.document.Item
import com.amazonaws.services.dynamodbv2.model.{ AttributeDefinition, CreateTableRequest, KeySchemaElement, KeyType, ProvisionedThroughput, ScalarAttributeType }

case class DynamoFlavor(
    customAdapters: List[TypeAdapterFactory] = List.empty[TypeAdapterFactory],
    hintMap:        Map[Type, String]        = Map.empty[Type, String],
    hintModifiers:  Map[Type, HintModifier]  = Map.empty[Type, HintModifier],
    parseOrElseMap: Map[Type, Type]          = Map.empty[Type, Type],
    defaultHint:    String                   = "_hint",
    isCanonical:    Boolean                  = true
) extends ScalaJackLike[Item] {

  def withAdapters(ta: TypeAdapterFactory*) = this.copy(customAdapters = this.customAdapters ++ ta.toList)
  def withHints(h: (Type, String)*) = this.copy(hintMap = this.hintMap ++ h)
  def withHintModifiers(hm: (Type, HintModifier)*) = this.copy(hintModifiers = this.hintModifiers ++ hm)
  def withDefaultHint(hint: String) = this.copy(defaultHint = hint)
  def parseOrElse(poe: (Type, Type)*) = this.copy(parseOrElseMap = this.parseOrElseMap ++ poe)
  def isCanonical(canonical: Boolean) = throw new UnsupportedOperationException("Not available for Dynamo formatting")

  // Embedded JSON-flavored ScalaJack, as Item can read/write JSON, so this is actually the most straightforward 
  // path to serialization.
  lazy val sj = ScalaJack()
    .withAdapters(customAdapters: _*)
    .withHints(hintMap.toList: _*)
    .withHintModifiers(hintModifiers.toList: _*)
    .withDefaultHint(defaultHint)
    .parseOrElse(parseOrElseMap.toList: _*)

  def read[T](item: Item)(implicit valueTypeTag: TypeTag[T]): T = {
    sj.read[T](item.toJSON())
  }

  def render[T](value: T)(implicit valueTypeTag: TypeTag[T]): Item = {
    Item.fromJSON(sj.render(value))
  }

  // This is Dynamo-Only.  User will have to case ScalaJack to DynamoFlavor to call this.
  def createTableRequest[T](provisionedThroughput: ProvisionedThroughput)(implicit tt: TypeTag[T]): CreateTableRequest = {
    val tpe = tt.tpe
    val typeAdapter = context.typeAdapter(tpe).asInstanceOf[CaseClassTypeAdapter[_]]
    val tableName = typeAdapter.collectionName.getOrElse(
      throw new java.lang.IllegalStateException(s"Class ${tpe.typeSymbol.fullName} must be annotated with @Collection to specify a table name.")
    )
    val keys: List[Member[_]] = typeAdapter.getDbKeys()
    if (keys.isEmpty) throw new java.lang.IllegalStateException(s"Class ${tpe.typeSymbol.fullName} must define at least a primary key with @DBKey.")
    // val (keySchema, attrDefs) = keys.zipWithIndex.collect {
    val attrDetail = keys.zipWithIndex.collect {
      case (key, idx) if (idx == 0) ⇒ (new AttributeDefinition(key.name, getAttrType(key)), new KeySchemaElement(key.name, KeyType.HASH))
      case (key, idx) if (idx == 1) ⇒ (new AttributeDefinition(key.name, getAttrType(key)), new KeySchemaElement(key.name, KeyType.RANGE))
    }.toList
    new CreateTableRequest(attrDetail.map(_._1).asJava, tableName, attrDetail.map(_._2).asJava, provisionedThroughput)
  }

  private def getAttrType(key: Member[_]) =
    if (key.valueTypeAdapter.isInstanceOf[StringKind])
      ScalarAttributeType.S
    else
      ScalarAttributeType.N

}
