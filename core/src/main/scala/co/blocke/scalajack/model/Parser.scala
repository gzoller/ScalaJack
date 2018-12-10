package co.blocke.scalajack
package model

trait Parser {
  def parse[PARSER_STATE, AST](ps: PARSER_STATE): AST // Wire reader
}

trait ArrayParser[E] extends Parser {
  val elementTypeAdapter: TypeAdapter[E]
}

trait EmitterState[WIRE] {

  def emit(prim: AST_PRIMITIVE, isMapKey: Boolean): EmitterState[WIRE]

  def addArray(a: List[AST_PRIMITIVE], isMapKey: Boolean): EmitterState[WIRE]
  def addBoolean(b: Boolean, isMapKey: Boolean): EmitterState[WIRE]
  def addInt(i: BigInt, isMapKey: Boolean): EmitterState[WIRE]
  def addDecimal(d: BigDecimal, isMapKey: Boolean): EmitterState[WIRE]
  def addMap(m: Map[AST_PRIMITIVE, AST_PRIMITIVE], isMapKey: Boolean): EmitterState[WIRE]
  def addNull(isMapKey: Boolean): EmitterState[WIRE]
  def addString(s: String, isMapKey: Boolean): EmitterState[WIRE]

  def result: WIRE
}

class UnexpectedException(ta: TypeAdapter[_], msg: String, index: Int = -1) extends Exception(msg)