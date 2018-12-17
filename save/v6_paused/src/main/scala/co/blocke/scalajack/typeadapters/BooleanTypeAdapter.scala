package co.blocke.scalajack
package typeadapters

import util.Path
import model._

object BooleanTypeAdapterFactory extends TypeAdapter.=:=[Boolean] {

  override def read[AST](path: Path, ast: AST)(implicit ops: Ops[AST], g: SerializationGuidance) = ast match {
    case AstBoolean(b) => b
    case AstNull()     => throw new ReadException(path, s"Null value read (Boolean is a non-nullable type)")
    case x             => throw new ReadException(path, s"Expected a Boolean value but read $x")
  }

  override def write[AST](t: Boolean)(implicit ops: Ops[AST], g: SerializationGuidance): AST = AstBoolean(t)
}
