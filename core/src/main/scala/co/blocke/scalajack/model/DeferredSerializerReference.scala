package co.blocke.scalajack
package model

class DeferredSerializerReference(resolve: () => Parser) extends Parser {

  private lazy val parser: Parser = resolve()

  override def parse[PARSER_STATE, AST](ps: PARSER_STATE): AST = resolve().parse(ps).asInstanceOf[AST]
}