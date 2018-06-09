package co.blocke.scalajack

import co.blocke.scalajack.Path.{ Element, Field }

object Path {

  case object Root extends Path {
    override def toString: String = "$"
  }

  case class Field(parent: Path, name: String) extends Path {
    override def toString: String = s"$parent.$name"
  }

  case class Element(parent: Path, index: Int) extends Path {
    override def toString: String = s"$parent[$index]"
  }

}

sealed trait Path {

  def \(fieldName: String): Field = Field(this, fieldName)

  def \(elementIndex: Int): Element = Element(this, elementIndex)

}
