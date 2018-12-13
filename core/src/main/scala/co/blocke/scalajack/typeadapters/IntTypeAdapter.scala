package co.blocke.scalajack
package typeadapters

import util.Path
import model._

object IntTypeAdapterFactory extends TypeAdapter.=:=[Int] {

  override def read[AST](path: Path, ast: AST)(implicit ops: Ops[AST], g: SerializationGuidance) = ast match {
    case AstInt(i) => i.intValue()
    case AstLong(longValue) if (longValue >= -2147483648 && longValue <= 2147483647) => longValue.toInt
    case AstLong(_) => throw new ReadException(path, s"Int value is out of range")
    case AstDouble(d) if d.isValidInt => d.toInt
    case AstDecimal(d) if (d.isValidInt) => d.toIntExact
    case AstNull() => throw new ReadException(path, s"Null value read (Int is a non-nullable type)")
    case x => throw new ReadException(path, s"Expected an Int value but read $x")
  }

  override def write[AST](t: Int)(implicit ops: Ops[AST], g: SerializationGuidance): AST = AstInt(BigInt(t))
}
