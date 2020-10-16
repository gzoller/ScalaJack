package co.blocke.scalajack
package yaml

import org.snakeyaml.engine.v2.nodes.Node
import scala.collection.mutable

case class YamlBuilder() extends mutable.Builder[Node, Node] {
  private var internalValue: Option[Node] = None

  def addOne(elem: Node): this.type = {
    internalValue = Some(elem)
    this
  }

  def clear(): Unit = internalValue = None

  def result(): Node = internalValue.getOrElse(
    throw new ScalaJackError("No value set for internal yaml builder")
  )
}