package co.blocke.scalajack
package model

import typeadapter._
/*
import typeadapter.javatime._
import typeadapter.javaprimitives._
import typeadapter.javacollections._
*/

import java.util.concurrent.ConcurrentHashMap

import util.TypeTags

import scala.reflect.runtime.currentMirror
import scala.util.{ Success, Try }

object Context {

  val StandardFactories: List[TypeAdapterFactory] = List(
    BigDecimalTypeAdapterFactory,
    BigIntTypeAdapterFactory,
    BinaryTypeAdapterFactory,
    BooleanTypeAdapterFactory,
    ByteTypeAdapterFactory,
    CharTypeAdapterFactory,
    DoubleTypeAdapterFactory,
    FloatTypeAdapterFactory,
    IntTypeAdapterFactory,
    LongTypeAdapterFactory,
    ShortTypeAdapterFactory,
    StringTypeAdapterFactory,
    TypeTypeAdapterFactory,
    TypeParameterTypeAdapterFactory,
    OptionTypeAdapterFactory,
    TryTypeAdapterFactory,
    TupleTypeAdapterFactory,
    EitherTypeAdapterFactory,
    EnumerationTypeAdapterFactory, // Either must precede SealedTraitTypeAdapter

    // WARNING: These two must precede CaseClassTypeAdapter in this list or all
    //     ValueClasses will be interpreted as case classes, and case objects
    //     will likewise be hidden (interpreted as regular classes).
    SealedTraitTypeAdapterFactory,
    classes.ValueClassTypeAdapterFactory,

    classes.CaseClassTypeAdapterFactory,
    classes.TraitTypeAdapterFactory,
    UUIDTypeAdapterFactory,
    AnyTypeAdapterFactory,
    JavaBigDecimalTypeAdapterFactory,
    JavaBigIntegerTypeAdapterFactory,
    JavaBooleanTypeAdapterFactory,
    JavaByteTypeAdapterFactory,
    JavaCharacterTypeAdapterFactory,
    JavaDoubleTypeAdapterFactory,
    JavaFloatTypeAdapterFactory,
    JavaIntTypeAdapterFactory,
    JavaLongTypeAdapterFactory,
    JavaNumberTypeAdapterFactory,
    JavaShortTypeAdapterFactory,
    DurationTypeAdapterFactory,
    InstantTypeAdapterFactory,
    LocalDateTimeTypeAdapterFactory,
    LocalDateTypeAdapterFactory,
    LocalTimeTypeAdapterFactory,
    OffsetDateTimeTypeAdapterFactory,
    OffsetTimeTypeAdapterFactory,
    PeriodTypeAdapterFactory,
    ZonedDateTimeTypeAdapterFactory,
    classes.PlainClassTypeAdapterFactory
  )
}

case class Context(factories: List[TypeAdapterFactory] = Nil) {

  sealed trait Phase
  case object Uninitialized extends Phase
  case object Initializing extends Phase
  case class Initialized(typeAdapterAttempt: Try[TypeAdapter[_]]) extends Phase

  object TypeEntryFactory extends java.util.function.Function[Type, TypeEntry] {
    override def apply(tpe: Type): TypeEntry = new TypeEntry(tpe)
  }

  class TypeEntry(tpe: Type) {

    @volatile
    private var phase: Phase = Uninitialized

    def typeAdapter: TypeAdapter[_] = {
      val attempt =
        phase match {
          case Initialized(a) =>
            a

          case Uninitialized | Initializing =>
            synchronized {
              phase match {
                case Uninitialized =>
                  phase = Initializing
                  val typeAdapterAttempt = Try {
                    val context: Context = Context.this
                    val tt: TypeTag[Any] = TypeTags.of(currentMirror, tpe)
                    val head :: tail = factories
                    head.typeAdapterOf(next = TypeAdapterFactory(tail))(context, tt)
                  }
                  phase = Initialized(typeAdapterAttempt)
                  typeAdapterAttempt

                case Initializing =>
                  Success(LazyTypeAdapter(Context.this, tpe))

                case Initialized(a) =>
                  a
              }
            }
        }

      attempt.get
    }

  }

  private val typeEntries = new ConcurrentHashMap[Type, TypeEntry]

  def withFactory(factory: TypeAdapterFactory): Context = copy(factories = factories :+ factory)

  def typeAdapter(tpe: Type): TypeAdapter[_] = typeEntries.computeIfAbsent(tpe, TypeEntryFactory).typeAdapter
  //def typeAdapterOf[T: TypeTag]: TypeAdapter[T] = typeAdapter(implicitly[TypeTag[T]].tpe).asInstanceOf[TypeAdapter[T]]
  def typeAdapterOf[T](implicit valueTypeTag: TypeTag[T]): TypeAdapter[T] =
    typeAdapter(valueTypeTag.tpe).asInstanceOf[TypeAdapter[T]]
}

