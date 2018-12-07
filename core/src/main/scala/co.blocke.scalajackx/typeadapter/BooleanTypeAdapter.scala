package co.blocke.scalajackx
package typeadapter

case class BooleanTypeAdapter() extends TypeAdapter[Boolean] {
  val serializer: Serializer
  val materializeFn: serializer.AST => T
  val dematerializeFn: T => serializer.AST
}
