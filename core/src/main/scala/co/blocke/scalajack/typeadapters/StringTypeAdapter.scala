package co.blocke.scalajack
package typeadapters

import util.Path
import model._

object StringTypeAdapterFactory extends TypeAdapter.=:=[String] {

  override def read[AST](path: Path, ast: AST)(implicit ops: Ops[AST], g: SerializationGuidance) = ast match {
    case AstString(s) => s
    case AstNull()    => null
    case x            => throw new ReadException(path, s"Expected a String value but read $x")
  }

  override def write[AST](t: String)(implicit ops: Ops[AST], g: SerializationGuidance): AST = AstString(t)
}
