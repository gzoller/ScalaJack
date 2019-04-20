package co.blocke.scalajack
package typeadapter

import model.TypeAdapter

// A TypeAdapter for a type T, which is wrapped in a String, a.k.a. "stringified".
// This is used for JSON Map keys, which must be strings.
trait StringWrapTypeAdapter[T] {
  val wrappedTypeAdapter: TypeAdapter[T]
}
