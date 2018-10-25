package co.blocke.scalajack
package typeadapter

trait BackedByAstValue {

  type BackingAstValue
  type SrcType

  def backingAstValue: BackingAstValue

  //  def backingAstValueAs[A: AstOps]: A =
  def backingAstValueAs[AST, S]()(implicit ops: AstOps[AST, S]): AST =
    AstValue.transform[BackingAstValue, AST, SrcType, S](backingAstValue)

  implicit def backingAstOps: AstOps[BackingAstValue, SrcType]

}
