package co.blocke.scalajack

// Let's play with macros!
import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros
import scala.reflect.runtime.universe._

object MyMacro {

  def readWrite[T](readParse: String => T, label: String, format: T => String): Any = macro readWriteImpl[T]

  def readWriteImpl[T: c.WeakTypeTag](c: Context)(readParse: c.Expr[String => T], label: c.Expr[String], format: c.Expr[T => String]): c.Expr[Any] = {
    import c.universe._

    //    def termName(s: c.Expr[String]): TermName = s.tree match {
    //      case Literal(Constant(s: String)) => TermName(s)
    //      case _ => c.abort(c.enclosingPosition, "Not a string literal")
    //    }

    val quasi =
      q"""
      new TypeAdapterFactory with TypeAdapter[${implicitly[c.WeakTypeTag[T]].tpe}] {
        def create(next: TypeAdapterFactory)(implicit tt: TypeTag[${implicitly[c.WeakTypeTag[T]].tpe}]): TypeAdapter[${implicitly[c.WeakTypeTag[T]].tpe}] = this

        implicit val typeTagT: TypeTag[${implicitly[c.WeakTypeTag[T]].tpe}] = TypeTags.of[${implicitly[c.WeakTypeTag[T]].tpe}](typeOf[${implicitly[c.WeakTypeTag[T]].tpe}])

        override def typeAdapterOf[U](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[U]): TypeAdapter[U] =
          if (tt.tpe =:= typeTagT.tpe)
            create(next)(tt.asInstanceOf[TypeTag[${implicitly[c.WeakTypeTag[T]].tpe}]]).asInstanceOf[TypeAdapter[U]]
          else
            next.typeAdapterOf[U]

        def read[WIRE](path: Path, reader: Transceiver[WIRE], isMapKey: Boolean = false): ${implicitly[c.WeakTypeTag[T]].tpe} =
          reader.readString(path) match {
            case null => null.asInstanceOf[${implicitly[c.WeakTypeTag[T]].tpe}]
            case s => Try( ${readParse.splice.apply("foo")} ) match {
              case Success(d) => d
              case Failure(u) => throw new ReadMalformedError(path, "Failed to parse "+${label.splice}+" from input '"+s+"'", List.empty[String], u)
            }
          }

        def write[WIRE](t: ${implicitly[c.WeakTypeTag[T]].tpe}, writer: Transceiver[WIRE], out: Builder[Any, WIRE]): Unit =
          t match {
            case null => writer.writeNull(out)
            case _    => writer.writeString($format(t), out)
          }
      }"""

    /*
    new TypeAdapterFactory with TypeAdapter[${implicitly[c.WeakTypeTag[T]].tpe}]{
      def create(next: TypeAdapterFactory)(implicit tt: TypeTag[${implicitly[c.WeakTypeTag[T]].tpe}]): TypeAdapter[${implicitly[c.WeakTypeTag[T]].tpe}] = this

      override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
        if (tt.tpe =:= ${implicitly[c.WeakTypeTag[T]].tpe})
          create(next)(tt.asInstanceOf[TypeTag[${implicitly[c.WeakTypeTag[T]].tpe}]]).asInstanceOf[TypeAdapter[T]]
        else
          next.typeAdapterOf[T]

      def read[WIRE](path: Path, reader: Transceiver[WIRE], isMapKey: Boolean = false): ${implicitly[c.WeakTypeTag[T]].tpe} =
        reader.readString(path) match {
          case null => null.asInstanceOf[${implicitly[c.WeakTypeTag[T]].tpe}]
          case s => Try( $readParse(s) ) match {
            case Success(d) => d
            case Failure(u) => throw new ReadMalformedError(path, "Failed to parse "+${termName(label)}+" from input '"+s+"'", List.empty[String], u)
          }
        }

      def write[WIRE](t: ${implicitly[c.WeakTypeTag[T]].tpe}, writer: Transceiver[WIRE], out: Builder[Any, WIRE]): Unit =
        t match {
          case null => writer.writeNull(out)
          case _    => writer.writeString($format(t), out)
        }
    }
           */

    c.Expr[Any](quasi)
  }
}
