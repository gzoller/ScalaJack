package co.blocke.scalajack
package model

import typeadapter._
import util.Path
import scala.util.{ Try, Success }

trait FlavorMaker {
  type WIRE
  def make(): JackFlavor[WIRE]
}

trait JackFlavor[WIRE] extends ViewSplice {

  def parse(wire: WIRE): Transceiver[WIRE]

  def read[T](wire: WIRE)(implicit tt: TypeTag[T]): T = {
    val p = parse(wire)
    val v = context.typeAdapter(tt.tpe).read(Path.Root, p).asInstanceOf[T]
    if (!p.isDone())
      throw new ReadInvalidError(Path.Root, "Extra input after read.\n" + p.showError(1))
    v
  }
  private def _read[T](p: Transceiver[WIRE])(implicit tt: TypeTag[T]): T = {
    val v = context.typeAdapter(tt.tpe).read(Path.Root, p).asInstanceOf[T]
    if (!p.isDone())
      throw new ReadInvalidError(Path.Root, "Extra input after read.\n" + p.showError(1))
    v
  }
  //  def fastRead(wire: WIRE): N = nativeTypeAdapter.read(Path.Root, parse(wire), false)

  def render[T](t: T)(implicit tt: TypeTag[T]): WIRE

  def filter[T](hintLabel: String = "")(implicit tt: TypeTag[T]): PartialFunction[Transceiver[WIRE], Option[T]] = {
    case p: Transceiver[WIRE] if hintLabel.length == 0 =>
      p.reset()
      Try(_read(p)(tt)).toOption
    case p: Transceiver[WIRE] if (hintLabel.length > 0) && {
      p.reset()

      val result = p.lookAheadForField(hintLabel) match {
        case Some(hintValue) =>
          p.jackFlavor.typeValueModifier match {
            case Some(fn) => // apply type value modifier if there is one (may explode!)
              try {
                val foundType = fn.apply(hintValue)
                (tt.tpe.typeArgs.size > 0 && tt.tpe.typeArgs.head == foundType) || foundType.baseClasses.contains(tt.tpe.typeSymbol)
              } catch {
                case _: Throwable => false // attempt to modify failed somehow
              }
            case None => Try(p.jackFlavor.typeTypeAdapter.read(Path.Root, p)) match {
              case Success(foundType) =>
                (tt.tpe.typeArgs.size > 0 && tt.tpe.typeArgs.head == foundType) || foundType.baseClasses.contains(tt.tpe.typeSymbol)
              case _ => false
            }
          }
        case None => false
      }
      p.rollbackToSave()
      result
    } =>
      p.reset()
      Some(_read(p)(tt))
    case _ => None
  }

  val defaultHint: String = "_hint"
  val stringifyMapKeys: Boolean = false
  val customAdapters: List[TypeAdapterFactory] = List.empty[TypeAdapterFactory]
  val hintMap: Map[Type, String] = Map.empty[Type, String]
  val hintValueModifiers: Map[Type, HintValueModifier] = Map.empty[Type, HintValueModifier]
  val typeValueModifier: Option[HintValueModifier] = None
  val parseOrElseMap: Map[Type, Type] = Map.empty[Type, Type]
  val permissivesOk: Boolean = false
  val enumsAsInt: Boolean = false

  /*
    val typeModifier: Option[HintModifier]
    def withTypeModifier(tm: HintModifier): ScalaJackLike[IR, WIRE]
    */
  def withAdapters(ta: TypeAdapterFactory*): JackFlavor[WIRE]
  def withDefaultHint(hint: String): JackFlavor[WIRE]
  def withHints(h: (Type, String)*): JackFlavor[WIRE]
  def withHintModifiers(hm: (Type, HintValueModifier)*): JackFlavor[WIRE]
  def withTypeValueModifier(tm: HintValueModifier): JackFlavor[WIRE]
  def parseOrElse(poe: (Type, Type)*): JackFlavor[WIRE]
  def allowPermissivePrimitives(): JackFlavor[WIRE]
  def enumsAsInts(): JackFlavor[WIRE]

  val context: Context = bakeContext()

  // These is so pervasively handy, let's just pre-stage it for easy access
  lazy val stringTypeAdapter = context.typeAdapterOf[String]
  lazy val typeTypeAdapter = context.typeAdapterOf[Type]
  lazy val anyTypeAdapter = context.typeAdapterOf[Any]

  // Look up any custom hint label for given type, and if none then use default
  def getHintLabelFor(tpe: Type) =
    hintMap.get(tpe).getOrElse(defaultHint)

  //  def getHintValueForType(traitType: Type, origValue: String): Type =
  //    hintValueModifiers.get(traitType).map(_.apply(origValue)).getOrElse(???)

  //  def forType[N2](implicit tt: TypeTag[N2]): JackFlavor[N2, WIRE]
  //  val nativeTypeAdapter: TypeAdapter[N]

  protected def bakeContext(): Context = {

    val intermediateContext = Context((customAdapters ::: Context.StandardFactories) :+ CanBuildFromTypeAdapterFactory(enumsAsInt))

    val permissives = if (permissivesOk)
      List(
        PermissiveBigDecimalTypeAdapterFactory,
        PermissiveBigIntTypeAdapterFactory,
        PermissiveBooleanTypeAdapterFactory,
        PermissiveByteTypeAdapterFactory,
        PermissiveDoubleTypeAdapterFactory,
        PermissiveFloatTypeAdapterFactory,
        PermissiveIntTypeAdapterFactory,
        PermissiveLongTypeAdapterFactory,
        PermissiveShortTypeAdapterFactory,
        PermissiveJavaBigDecimalTypeAdapterFactory,
        PermissiveJavaBigIntegerTypeAdapterFactory,
        PermissiveJavaBooleanTypeAdapterFactory,
        PermissiveJavaByteTypeAdapterFactory,
        PermissiveJavaDoubleTypeAdapterFactory,
        PermissiveJavaFloatTypeAdapterFactory,
        PermissiveJavaIntTypeAdapterFactory,
        PermissiveJavaLongTypeAdapterFactory,
        PermissiveJavaNumberTypeAdapterFactory,
        PermissiveJavaShortTypeAdapterFactory
      )
    else
      List.empty[TypeAdapterFactory]

    // ParseOrElse functionality
    val parseOrElseFactories = parseOrElseMap.map {
      case (attemptedType, fallbackType @ _) =>
        val attemptedTypeAdapter = intermediateContext.typeAdapter(attemptedType)
        val fallbackTypeAdapter = intermediateContext.typeAdapter(fallbackType)

        new TypeAdapterFactory {
          override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, typeTag: TypeTag[T]): TypeAdapter[T] =
            if (typeTag.tpe =:= attemptedType) {
              val primary = attemptedTypeAdapter.asInstanceOf[TypeAdapter[T]]
              val secondary = fallbackTypeAdapter.asInstanceOf[TypeAdapter[T]]
              FallbackTypeAdapter[T, T](Some(primary), secondary)
            } else {
              next.typeAdapterOf[T]
            }
        }
    }.toList

    val ctx = intermediateContext.copy(factories = parseOrElseFactories ::: permissives ::: intermediateContext.factories)

    // A little wiring to inject JackFlavor into a few places
    AnyTypeAdapterFactory.jackFlavor = this

    ctx
  }
}
