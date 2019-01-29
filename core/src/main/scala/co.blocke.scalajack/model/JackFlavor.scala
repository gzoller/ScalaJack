package co.blocke.scalajack
package model

import typeadapter.{ AnyTypeAdapterFactory, FallbackTypeAdapter }
import typeadapter.classes.TraitTypeAdapterFactory
import util.Path

trait JackFlavor[N, WIRE] {

  def parse(wire: WIRE): Transceiver[WIRE]

  def read[T](wire: WIRE)(implicit tt: TypeTag[T]): T = context.typeAdapter(tt.tpe).read(Path.Root, parse(wire)).asInstanceOf[T]
  //  def fastRead(wire: WIRE): N = nativeTypeAdapter.read(Path.Root, parse(wire), false)

  def render[T](t: T)(implicit tt: TypeTag[T]): WIRE

  val defaultHint: String = "_hint"
  val customAdapters: List[TypeAdapterFactory] = List.empty[TypeAdapterFactory]
  val hintMap: Map[Type, String] = Map.empty[Type, String]
  val hintValueModifiers: Map[Type, HintValueModifier] = Map.empty[Type, HintValueModifier]
  val parseOrElseMap: Map[Type, Type] = Map.empty[Type, Type]

  /*
    val customAdapters: List[TypeAdapterFactory]
    val hintMap: Map[Type, String]
    val hintModifiers: Map[Type, HintModifier]
    val parseOrElseMap: Map[Type, Type]
    val isCanonical: Boolean
    val typeModifier: Option[HintModifier]
    val secondLookParsing: Boolean

    def withTypeModifier(tm: HintModifier): ScalaJackLike[IR, WIRE]
    def isCanonical(canonical: Boolean): ScalaJackLike[IR, WIRE]
    */
  def withAdapters(ta: TypeAdapterFactory*): JackFlavor[N, WIRE]
  def withDefaultHint(hint: String): JackFlavor[N, WIRE]
  def withHints(h: (Type, String)*): JackFlavor[N, WIRE]
  def withHintModifiers(hm: (Type, HintValueModifier)*): JackFlavor[N, WIRE]
  def withSecondLookParsing(): JackFlavor[N, WIRE]
  def parseOrElse(poe: (Type, Type)*): JackFlavor[N, WIRE]

  val context: Context = bakeContext()

  // This is so pervasively handy, let's just pre-stage it for easy access
  val stringTypeAdapter = context.typeAdapterOf[String]

  // Look up any custom hint label for given type, and if none then use default
  def getHintLabelFor(tpe: Type) =
    hintMap.get(tpe).getOrElse(defaultHint)

  //  def getHintValueForType(traitType: Type, origValue: String): Type =
  //    hintValueModifiers.get(traitType).map(_.apply(origValue)).getOrElse(???)

  //  def forType[N2](implicit tt: TypeTag[N2]): JackFlavor[N2, WIRE]
  //  val nativeTypeAdapter: TypeAdapter[N]

  /**
   * Project fields from given master object to a view object of type T.  Field names/types must match master
   * precisely.
   * @param master the master object from which the smaller object is projected
   * @return an object of type T which is a "subset" of the master
   */
  /*
    def view[T](master: Any)(implicit tt: TypeTag[T]): T =
      if (tt.tpe.typeSymbol.asClass.isCaseClass)
        (dematerialize(master) match {
          case WriteSuccess(w)  => materialize[T](w)
          case wf: WriteFailure => throw new ViewException(wf.toString)
        }) match {
          case ReadSuccess(t)  => t.get
          case rf: ReadFailure => throw new ViewException(rf.toString)
        }
      else
        throw new ViewException(s"Output of view() must be a case class, not ${tt.tpe}")
        */

  /**
   * Splice a view (subset) object's fields into a master object's fields.
   * @param view the subset object
   * @param master master object
   * @return the master object with the view object's corresponding fields merged/overlayed
   */
  /*
    def spliceInto[T, U](view: T, master: U)(implicit tt: TypeTag[T], tu: TypeTag[U]): U = {
      val viewIR = (dematerialize(view) match {
        case WriteSuccess(ws) => ws match {
          case IRObject(x) => x
          case _           => throw new ViewException(s"View must be a case class, not ${tt.tpe}")
        }
        // $COVERAGE-OFF$Not sure how to trigger this! Here for extra safety, really.
        case wf: WriteFailure => throw new ViewException(wf.toString)
        // $COVERAGE-ON$
      }).toMap
      val masterIR = (dematerialize(master) match {
        case WriteSuccess(ws) => ws match {
          case IRObject(x) => x
          case _           => throw new ViewException(s"Master must be a case class, not ${tu.tpe}")
        }
        // $COVERAGE-OFF$Not sure how to trigger this! Here for extra safety, really.
        case wf: WriteFailure => throw new ViewException(wf.toString)
        // $COVERAGE-ON$
      }).toMap
      val newMaster = masterIR.map { case (name, value) => (name, viewIR.getOrElse(name, value)) }.toSeq
      materialize[U](ops.applyObject(newMaster)) match {
        case ReadSuccess(t)  => t.get
        // $COVERAGE-OFF$Not sure how to trigger this! Here for extra safety, really.
        case rf: ReadFailure => throw new ViewException(rf.toString)
        // $COVERAGE-ON$
      }
    }
    */

  protected def bakeContext(): Context = {

    val intermediateContext = Context(customAdapters ::: Context.StandardFactories)

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
              FallbackTypeAdapter[T, T](primary, secondary)
            } else {
              next.typeAdapterOf[T]
            }
        }
    }.toList

    val ctx = intermediateContext.copy(factories = parseOrElseFactories ::: intermediateContext.factories)

    // A little wiring to inject JackFlavor into a few places
    AnyTypeAdapterFactory.jackFlavor = this
    TraitTypeAdapterFactory.jackFlavor = this

    ctx
  }
}
