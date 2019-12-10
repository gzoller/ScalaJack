package co.blocke.scalajack
package model

import typeadapter._
import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror
import scala.util.{ Success, Try }

import util.TypeTags

object TypeAdapterCache {

  val StandardFactories: List[TypeAdapterFactory] =
    List(
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
      //    TypeTypeAdapterFactory,
      TypeParameterTypeAdapterFactory,
      OptionTypeAdapterFactory,
      TryTypeAdapterFactory,
      TupleTypeAdapterFactory,
      EitherTypeAdapterFactory, // Either must precede SealedTraitTypeAdapter
      UnionTypeAdapterFactory,
      EnumerationTypeAdapterFactory,
      // WARNING: These two must precede CaseClassTypeAdapter in this list or all
      //     ValueClasses will be interpreted as case classes, and case objects
      //     will likewise be hidden (interpreted as regular classes).
      SealedTraitTypeAdapterFactory,
      ValueClassTypeAdapterFactory,
      ClassTypeAdapterFactory,
      TraitTypeAdapterFactory,
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
      PlainClassTypeAdapterFactory
    )
}

case class TypeAdapterCache(
    jackFlavor: JackFlavor[_],
    factories:  List[TypeAdapterFactory] = Nil) {

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
                    val taCache: TypeAdapterCache = TypeAdapterCache.this
                    val tt: TypeTag[Any] = TypeTags.of(currentMirror, tpe)
                    val head :: tail = factories
                    head.typeAdapterOf(next = TypeAdapterFactory(tail))(
                      taCache,
                      tt
                    )
                  }
                  phase = Initialized(typeAdapterAttempt)
                  typeAdapterAttempt

                case Initializing =>
                  Success(LazyTypeAdapter(TypeAdapterCache.this, tpe))

                case Initialized(a) =>
                  a
              }
            }
        }

      attempt.get
    }

  }

  private val typeEntries =
    new java.util.concurrent.ConcurrentHashMap[Type, TypeEntry]

  def withFactory(factory: TypeAdapterFactory): TypeAdapterCache =
    copy(factories = factories :+ factory)

  def typeAdapter(tpe: Type): TypeAdapter[_] =
    typeEntries.computeIfAbsent(tpe, TypeEntryFactory).typeAdapter
  //def typeAdapterOf[T: TypeTag]: TypeAdapter[T] = typeAdapter(implicitly[TypeTag[T]].tpe).asInstanceOf[TypeAdapter[T]]
  def typeAdapterOf[T](implicit valueTypeTag: TypeTag[T]): TypeAdapter[T] =
    typeAdapter(valueTypeTag.tpe).asInstanceOf[TypeAdapter[T]]
}
