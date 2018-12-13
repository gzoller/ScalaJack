package co.blocke.scalajack
package model

import util.Path

/**
 * TypeAdapter includes two matching patterns you can use when you extend trait TypeAdapter for your
 * custom adapters.  The two matching behaviors are '===' and '=:='.
 *
 * This difference is because =:= matches children.  Consider:
 *
 *    type Phone = String
 *    case class( name:String, phone:Phone )
 *
 * With =:= both name and phone (String and Phone) will match a TypeAdapter derived from =:=.
 * This is actually what you want if you haven't overridden Phone with its own TypeAdapter... it should default
 * to the TypeAdapter of its base type.
 *
 * But... if you did provide an override PhoneTypeAdapter you want the matching to be strict, so we use  ===
 * in this case.  With strict matching String != Phone.
 *
 */
object TypeAdapter {

  abstract class ===[X](implicit ttFactory: TypeTag[X]) extends TypeAdapterFactory.===[X] with TypeAdapter[X] {
    override def create(next: TypeAdapterFactory)(implicit tt: TypeTag[X]): TypeAdapter[X] = this
  }

  abstract class =:=[X](implicit ttFactory: TypeTag[X]) extends TypeAdapterFactory.=:=[X] with TypeAdapter[X] {
    override def create(next: TypeAdapterFactory)(implicit tt: TypeTag[X]): TypeAdapter[X] = this
  }

}

trait TypeAdapter[T] {

  def read[AST](path: Path, ast: AST)(implicit ops: Ops[AST], g: SerializationGuidance): T =
    throw new UnsupportedOperationException("read is unsupported at this Path")
  def readFromNothing[AST](path: Path)(implicit ops: Ops[AST]): T =
    throw new UnsupportedOperationException("readFromNothing is unsupported at this Path")
  def write[AST](t: T)(implicit ops: Ops[AST], g: SerializationGuidance): AST =
    throw new UnsupportedOperationException("write is unsupported at this Path")

  def defaultValue: Option[T] = None

  def resolved: TypeAdapter[T] = this // Might be something else during Lazy construction
}

trait ArrayTypeAdapter[T, E] extends TypeAdapter[T] {
  val elementTypeAdapter: TypeAdapter[E]
}
