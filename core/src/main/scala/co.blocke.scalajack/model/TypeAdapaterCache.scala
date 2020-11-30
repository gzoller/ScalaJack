package co.blocke.scalajack
package model

import typeadapter._
import typeadapter.classes._
import scala.util.{ Success, Try }
import co.blocke.scala_reflection._
import co.blocke.scala_reflection.impl.SelfRefRType
import co.blocke.scala_reflection.info._


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
      OptionTypeAdapterFactory,
      TryTypeAdapterFactory,
      TupleTypeAdapterFactory,
      EitherTypeAdapterFactory, // Either must precede SealedTraitTypeAdapter
      UnionTypeAdapterFactory,
      IntersectionTypeAdapterFactory,
      ArrayTypeAdapterFactory,
      EnumTypeAdapterFactory,
      UUIDTypeAdapterFactory,
      CollectionTypeAdapterFactory,

      // WARNING: These two must precede CaseClassTypeAdapter in this list or all
      //     ValueClasses will be interpreted as case classes, and case objects
      //     will likewise be hidden (interpreted as regular classes).
      SealedTraitTypeAdapterFactory,
      ValueClassTypeAdapterFactory,
      ScalaClassTypeAdapterFactory,

      TraitTypeAdapterFactory,
      AnyTypeAdapterFactory,
      JavaBigDecimalTypeAdapterFactory,
      JavaBigIntegerTypeAdapterFactory,
      JavaBooleanTypeAdapterFactory,
      JavaByteTypeAdapterFactory,
      JavaCharacterTypeAdapterFactory,
      JavaDoubleTypeAdapterFactory,
      JavaFloatTypeAdapterFactory,
      JavaIntegerTypeAdapterFactory,
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
      JavaClassTypeAdapterFactory
    )  
}

case class TypeAdapterCache(
    jackFlavor: JackFlavor[_],
    factories:  List[TypeAdapterFactory]):

  sealed trait Phase
  case object Uninitialized extends Phase
  case object Initializing extends Phase
  case class Initialized(typeAdapterAttempt: Try[TypeAdapter[_]]) extends Phase

  val selfCache = this

  class TypeEntry(tpe: RType):
    @volatile
    private var phase: Phase = Uninitialized
    // println(s"--> TACache (${typeEntries.size}) add [${tpe.name}]")

    def typeAdapter: TypeAdapter[_] = 
      val attempt =
        phase match {
          case Initialized(a) => a

          case Uninitialized | Initializing =>
            synchronized {
              phase match {
                case Uninitialized =>
                  phase = Initializing
                  val typeAdapterAttempt = Try {
                    val foundFactory = factories.find(_.matches(tpe)).get
                    foundFactory.makeTypeAdapter(tpe)(selfCache)
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


  private val typeEntries = new java.util.concurrent.ConcurrentHashMap[RType, TypeEntry]

  def withFactory(factory: TypeAdapterFactory): TypeAdapterCache =
    copy(factories = factories :+ factory)

  // def typeAdapterOf(tpe: TypeStructure): TypeAdapter[_] = 
  //   typeAdapterOf(RType.ofType(tpe))

  def typeAdapterOf(concreteType: RType): TypeAdapter[_] =
    typeEntries.computeIfAbsent(concreteType, ConcreteTypeEntryFactory).typeAdapter

  inline def typeAdapterOf[T]: TypeAdapter[T] =
    typeAdapterOf(RType.of[T]).asInstanceOf[TypeAdapter[T]]

  val self = this 

  object ConcreteTypeEntryFactory extends java.util.function.Function[RType, TypeEntry]:
    private val AnyRType = RType.of[Any]
    private val AnySelfRef = SelfRefRType("scala.Any")
    override def apply(concrete: RType): TypeEntry = 
      concrete match {
        case AnySelfRef      => new TypeEntry(AnyRType)
        case s: SelfRefRType => new TypeEntry(RType.of(s.infoClass))
        case s               => new TypeEntry(s)
      }
