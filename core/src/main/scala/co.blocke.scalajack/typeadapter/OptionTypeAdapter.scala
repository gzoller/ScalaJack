package co.blocke.scalajack
package typeadapter

import model._

object OptionTypeAdapterFactory extends TypeAdapterFactory.=:=.withOneTypeParam[Option] {

  override def create[E](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[Option[E]], ttElement: TypeTag[E]): TypeAdapter[Option[E]] = {
    val valueTypeAdapter = context.typeAdapterOf[E]
    OptionTypeAdapter(valueTypeAdapter)
  }

}

// We need 3 types of Option adapters here:
//   1: The "normal" one writes nothing for None.  This eliminates the None from the structure, e.g. fields in objects.
//   2: "Empty" one writes "" for None.  This is used for Map keys that are None.
//   3: "Null" one converts None into null.  This is used mainly for Tuple members and Map values.
//

case class OptionTypeAdapter[E](valueTypeAdapter: TypeAdapter[E])(implicit tt: TypeTag[E], tu: TypeTag[Option[E]]) extends TypeAdapter[Option[E]] {

  override def defaultValue: Option[Option[E]] = Some(None)

  // Must be called by parent of the Option when appropriate to get the null-writing version.
  //  def noneAsNull: TypeAdapter[Option[E]] = OptionTypeAdapterNull(valueTypeAdapter)
  //  def noneAsEmptyString: TypeAdapter[Option[T]] = OptionTypeAdapterEmpty(valueTypeAdapter)

  private val SomeTypeConstructor: Type = typeOf[Some[_]].typeConstructor
  private val TaggedNone: None.type = None
  private val TaggedNull: None.type = null.asInstanceOf[None.type]

  private val OptionTypeSymbol: TypeSymbol = symbolOf[Option[_]]

  def read(reader: Reader, isMapKey: Boolean): Option[E] =
    valueTypeAdapter.read(reader, isMapKey) match {
      case null if isMapKey => null
      case null             => None
      case s: String if s == "" =>
        // Handle empty string.  If T is String type then conjure up Some("") else morph "" into None
        typeOf[E] match {
          case t if t == typeOf[String] && !isMapKey => Some("").asInstanceOf[Option[E]]
          case _                                     => None
        }
      case v =>
        Some(v)
    }

  /*
  override def read[AST](path: Path, ast: AST)(implicit ops: Ops[AST], g: SerializationGuidance) =
    ast match {
      case AstNull() if (g.isMapKey) =>
        null
      case AstNull() =>
        None
      case AstString(s) if (s == "") =>
        // Handle empty string.  If T is String type then conjure up Some("") else morph "" into None
        typeOf[E] match {
          case t if t == typeOf[String] && !g.isMapKey =>
            Some(valueTypeAdapter.read(path, ast))
          case _ => None
        }
      case _ =>
        Some(valueTypeAdapter.read(path, ast))
    }

  override def readFromNothing[AST](path: Path)(implicit ops: Ops[AST]): Option[E] = None.asInstanceOf[Option[E]]
*/
  //  override def write[AST](t: Option[E])(implicit ops: Ops[AST], g: SerializationGuidance): AST =
  //    t match {
  //      case null                              => AstNull()
  //      case None if (g.isMapKey)              => AstString("")
  //      case None if (g.inSeq || g.isMapValue) => AstNull()
  //      case None                              => throw new Exception("Boom") // This can be a "valid" failure.
  //      case Some(value)                       => valueTypeAdapter.write(value)
  //    }
}

//case class OptionTypeAdapterNull[E](valueTypeAdapter: TypeAdapter[E]) extends TypeAdapter[Option[E]]
