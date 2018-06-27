package co.blocke.scalajack
package typeadapter

trait BackedByJsonValue {

  type BackingJsonValue

  def backingJsonValue: BackingJsonValue

  def backingJsonValueAs[J: JsonOps]: J =
    JsonValue.transform[BackingJsonValue, J](backingJsonValue)

  implicit def backingJsonOps: JsonOps[BackingJsonValue]

}
