package co.blocke.scalajack
package dynamodb

import scala.collection.mutable
import scala.reflect.runtime.universe.{ Type, TypeTag }
import scala.reflect.runtime.currentMirror

import com.amazonaws.services.dynamodbv2.document.Item

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
}
