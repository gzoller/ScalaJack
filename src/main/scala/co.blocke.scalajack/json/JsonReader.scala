package co.blocke.scalajack
package json

import co.blocke.scala_reflection.{RTypeRef, TypedName}
import scala.quoted.*
import scala.collection.mutable.HashMap
import scala.util.{Failure, Success, Try}

case class JsonReader() extends ReaderModule:

  val root: ReaderModule = null // Should never be accessed--we're the root!

  // Did the user supply an extension module?
  val extension = Try(Class.forName("co.blocke.scalajack.json.ReaderExtension")) match
    case Success(c) => Some(c.getDeclaredConstructor().newInstance.asInstanceOf[ReaderModule])
    case Failure(_) => None

  val modules = readers.PrimitiveReader(
    readers.ColletionReader(
      readers.ClassReader(
        readers.EnumReader(
          readers.MiscReader(
            TerminusReaderModule(extension, root),
            root
          ),
          root
        ),
        this
      ),
      this
    ),
    this
  )

  def readerFn[T](ref: RTypeRef[T], isMapKey: Boolean = false)(using q: Quotes, tt: Type[T])(using cache: HashMap[Expr[TypedName], Expr[(JsonConfig, JsonParser) => Either[ParseError, ?]]]): Expr[(JsonConfig, JsonParser) => Either[ParseError, T]] =
    modules.readerFn[T](ref)

    // TODO:
    // * Enumeration
    // * Java Primitives
    // * Java Classes
    // * Java Collections
    // * Java Enums
    // * Non-case Scala classes
    // * SealedTraitRef
    // * TraitRef

  // -----------------------------------
