package co.blocke.scalajack
package model

trait JackFlavor[N, WIRE] {

  def parse(wire: WIRE): Reader

  def read[T](wire: WIRE)(implicit tt: TypeTag[T]): T = context.typeAdapter(tt.tpe).read(parse(wire)).asInstanceOf[T]
  def fastRead(wire: WIRE): N = nativeTypeAdapter.read(parse(wire))

  val defaultHint: String = "_hint"

  /*
    val customAdapters: List[TypeAdapterFactory]
    val hintMap: Map[Type, String]
    val hintModifiers: Map[Type, HintModifier]
    val parseOrElseMap: Map[Type, Type]
    val isCanonical: Boolean
    val typeModifier: Option[HintModifier]
    val secondLookParsing: Boolean

    def withAdapters(ta: TypeAdapterFactory*): ScalaJackLike[IR, WIRE]
    def withHints(h: (Type, String)*): ScalaJackLike[IR, WIRE]
    def withHintModifiers(hm: (Type, HintModifier)*): ScalaJackLike[IR, WIRE]
    def withDefaultHint(hint: String): ScalaJackLike[IR, WIRE]
    def withTypeModifier(tm: HintModifier): ScalaJackLike[IR, WIRE]
    def parseOrElse(poe: (Type, Type)*): ScalaJackLike[IR, WIRE]
    def isCanonical(canonical: Boolean): ScalaJackLike[IR, WIRE]
    def withSecondLookParsing(): ScalaJackLike[IR, WIRE]
    */

  val context: Context = bakeContext()

  def forType[N2](implicit tt: TypeTag[N2]): JackFlavor[N2, WIRE]
  val nativeTypeAdapter: TypeAdapter[N]

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
    /*
      // Types where either the label or the type value (or both) are modified
      val polymorphicTypes: Set[Type] = hintModifiers.keySet ++ hintMap.keySet

      val polymorphicTypeAdapterFactories = polymorphicTypes.map { polymorphicType: Type =>
        val hintLabel = hintMap.getOrElse(polymorphicType, defaultHint)
        val hintToType = hintModifiers.getOrElse(polymorphicType, fullNameToType)

        new TypeAdapterFactory {
          override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, typeTag: TypeTag[T]): TypeAdapter[T] = {
            if (typeTag.tpe.typeSymbol == polymorphicType.typeSymbol) {
              val typeTypeAdapter = context.typeAdapterOf[Type]
              TraitTypeAdapter[T](
                new TraitIRTransceiver[T](hintLabel, typeTypeAdapter.irTransceiver, Some(hintToType.memoized)),
                typeTag.tpe)
            } else
              next.typeAdapterOf[T]
          }
        }
      }.toList

      val typeModFactories = typeModifier.map(mod => List(new TypeAdapterFactory {
        override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] = {
          if (tt.tpe =:= typeOf[Type]) {
            TypeTypeAdapter(
              new TypeTypeIRTransceiver(mod.unapply, mod.apply),
              tt.mirror,
              Some(mod)).asInstanceOf[TypeAdapter[T]]
          } else {
            next.typeAdapterOf[T]
          }
        }
      })).getOrElse(List.empty[TypeAdapterFactory])

      val intermediateContext = Context(
        defaultHint,
        factories = customAdapters ::: typeModFactories ::: polymorphicTypeAdapterFactories ::: Context.StandardContext.factories ::: List(TraitTypeAdapterFactory(defaultHint), PlainClassTypeAdapter),
        Some(this))

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
                FallbackTypeAdapter[T](primary, secondary)
              } else {
                next.typeAdapterOf[T]
              }
          }
      }.toList

      intermediateContext.copy(factories = parseOrElseFactories ::: intermediateContext.factories)
      */

    Context(
      this,
      defaultHint,
      factories = Context.StandardFactories)
  }
}
