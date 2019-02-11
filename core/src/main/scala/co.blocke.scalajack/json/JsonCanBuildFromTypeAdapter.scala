package co.blocke.scalajack
package json

import typeadapter.CanBuildFromTypeAdapterFactoryPrototype

case class JsonCanBuildFromTypeAdapterFactory(override val enumsAsInt: Boolean) extends CanBuildFromTypeAdapterFactoryPrototype {
  val stringifyMapKeys = true
}