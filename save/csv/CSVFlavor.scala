package co.blocke.scalajack
package csv

import scala.reflect.runtime.universe.{Type, TypeTag}
import java.lang.{UnsupportedOperationException => UOE}

import typeadapter.DerivedValueClassAdapter

case class CSVFlavor() extends {
  val customAdapters: List[TypeAdapterFactory] = List.empty[TypeAdapterFactory]
  val hintMap: Map[Type, String] = Map.empty[Type, String]
  val hintModifiers: Map[Type, HintModifier] = Map.empty[Type, HintModifier]
  val typeModifier: Option[HintModifier] = None
  val parseOrElseMap: Map[Type, Type] = Map.empty[Type, Type]
  val defaultHint: String = "_hint"
  val isCanonical: Boolean = true
} with ScalaJackLike[String, ???] {

  def withAdapters(ta: TypeAdapterFactory*) = throw new UOE("Not available for CSV formatting")
  def withHints(h: (Type, String)*) = throw new UOE("Not available for CSV formatting")
  def withHintModifiers(hm: (Type, HintModifier)*) = throw new UOE("Not available for CSV formatting")
  def withDefaultHint(hint: String) = throw new UOE("Not available for CSV formatting")
  def withTypeModifier(tm: HintModifier) = throw new UOE("Not available for CSV formatting")
  def parseOrElse(poe: (Type, Type)*) = throw new UOE("Not available for CSV formatting")
  def isCanonical(canonical: Boolean) = throw new UOE("Not available for CSV formatting")

  override val context: Context = {
    val ctx = bakeContext()
    ctx.copy(factories = DerivedValueClassAdapter :: CSVCaseClassTypeAdapter :: ctx.factories)
  }

  def readSafely[T](csv: String)(implicit tt: TypeTag[T]): Either[DeserializationFailure, T] = {
    val tokenizer = new Tokenizer

    val source = csv.toCharArray
    val reader = tokenizer.tokenize(source, 0, source.length, source.length / 6) // The /6 is arbitrary guess characters:token ratio.

    context.typeAdapterOf[T].read(reader)
  }

  def render[T](value: T)(implicit valueTypeTag: TypeTag[T]): String = {
    val writer = new StringCSVWriter()
    context.typeAdapterOf[T].write(value, writer)
    writer.csvString
  }

  def parse(csv: String): AST
  def emit(ast: AST): String

}
