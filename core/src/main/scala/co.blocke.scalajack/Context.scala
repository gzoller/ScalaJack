package co.blocke.scalajack

import typeadapter._
import typeadapter.javatime._
import typeadapter.javaprimitives._

import scala.language.existentials
import scala.reflect.runtime.universe.{ Type, TypeTag }
import scala.reflect.runtime.currentMirror
import scala.util.{ Success, Try }

import java.util.concurrent.ConcurrentHashMap

object Context {

  val StandardContext = Context()
    .withFactory(TypeParameterTypeAdapter)
    .withFactory(AnyTypeAdapter)
    .withFactory(TypeTypeAdapter)
    .withFactory(CanBuildFromTypeAdapter)
    .withFactory(TupleTypeAdapter)

    .withFactory(DerivedValueClassAdapter) // <-- WARNING: This must preceed CaseClassTypeAdapter or all 
    //              ValueClasses will be interpreted as case classes!

    .withFactory(CaseClassTypeAdapter)
    .withFactory(OptionTypeAdapter)
    .withFactory(TryTypeAdapter)
    .withFactory(BooleanTypeAdapter)
    .withFactory(CharTypeAdapter)
    .withFactory(ByteTypeAdapter)
    .withFactory(ShortTypeAdapter)
    .withFactory(IntTypeAdapter)
    .withFactory(LongTypeAdapter)
    .withFactory(FloatTypeAdapter)
    .withFactory(DoubleTypeAdapter)
    .withFactory(BigDecimalTypeAdapter)
    .withFactory(BigIntTypeAdapter)
    .withFactory(StringTypeAdapter)
    .withFactory(EnumerationTypeAdapter)
    .withFactory(JavaNumberTypeAdapter)
    .withFactory(JavaBooleanTypeAdapter)
    .withFactory(JavaByteTypeAdapter)
    .withFactory(JavaCharacterTypeAdapter)
    .withFactory(JavaDoubleTypeAdapter)
    .withFactory(JavaFloatTypeAdapter)
    .withFactory(JavaIntegerTypeAdapter)
    .withFactory(JavaLongTypeAdapter)
    .withFactory(JavaShortTypeAdapter)
    .withFactory(JavaBigDecimalTypeAdapter)
    .withFactory(JavaBigIntegerTypeAdapter)
    .withFactory(UUIDTypeAdapter)
    .withFactory(DurationTypeAdapter)
    .withFactory(InstantTypeAdapter)
    .withFactory(LocalDateTimeTypeAdapter)
    .withFactory(LocalDateTypeAdapter)
    .withFactory(LocalTimeTypeAdapter)
    .withFactory(OffsetDateTimeTypeAdapter)
    .withFactory(OffsetTimeTypeAdapter)
    .withFactory(PeriodTypeAdapter)
    .withFactory(ZonedDateTimeTypeAdapter)
}

case class Context(defaultHint: String = "", factories: List[TypeAdapterFactory] = Nil) {

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
                    val head :: tail = factories
                    val chain = TypeAdapterFactoryChain(tail)
                    head.typeAdapterOf(chain)(Context.this, TypeTags.of(currentMirror, tpe))
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

  def withFactory(factory: TypeAdapterFactory): Context =
    copy(factories = factories :+ factory)

  def typeAdapter(tpe: Type): TypeAdapter[_] =
    typeEntries.computeIfAbsent(tpe, TypeEntryFactory).typeAdapter

  def typeAdapterOf[T](implicit valueTypeTag: TypeTag[T]): TypeAdapter[T] =
    typeAdapter(valueTypeTag.tpe).asInstanceOf[TypeAdapter[T]]

}
