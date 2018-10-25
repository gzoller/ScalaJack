package co.blocke.scalajack

import co.blocke.scalajack.BijectiveFunctions._
import co.blocke.scalajack.json.JsonFlavor
import co.blocke.scalajack.typeadapter.CaseClassTypeAdapter.FieldMember
import co.blocke.scalajack.typeadapter._

object ScalaJack {
  def apply[AST, S](kind: ScalaJackLike[AST, S] = JsonFlavor()): ScalaJackLike[AST, S] = kind
}

abstract class ScalaJackLike[AST, S] extends JackFlavor[AST, S] {
  val customAdapters: List[TypeAdapterFactory]
  val hintMap: Map[Type, String]
  val hintModifiers: Map[Type, HintModifier]
  val parseOrElseMap: Map[Type, Type]
  val defaultHint: String
  val isCanonical: Boolean
  val typeModifier: Option[HintModifier]
  val secondLookParsing: Boolean

  val context: Context = bakeContext()

  def withAdapters(ta: TypeAdapterFactory*): ScalaJackLike[AST, S]
  def withHints(h: (Type, String)*): ScalaJackLike[AST, S]
  def withHintModifiers(hm: (Type, HintModifier)*): ScalaJackLike[AST, S]
  def withDefaultHint(hint: String): ScalaJackLike[AST, S]
  def withTypeModifier(tm: HintModifier): ScalaJackLike[AST, S]
  def parseOrElse(poe: (Type, Type)*): ScalaJackLike[AST, S]
  def isCanonical(canonical: Boolean): ScalaJackLike[AST, S]
  def withSecondLookParsing(): ScalaJackLike[AST, S]

  /**
   * Project fields from given master object to a view object of type T.  Field names/types must match master
   * precisely.
   * @param master the master object from which the smaller object is projected
   * @return an object of type T which is a "subset" of the master
   */
  // WARNING: Assumes CaseClassTypeAdapter.members is in constructor-order.  If not, sort on members.index.
  def view[T](master: Any)(implicit tt: TypeTag[T]): T = {
    val viewTarget = context.typeAdapter(tt.tpe).maybeAs[CaseClassTypeAdapter[T]] match {
      case Some(ta: CaseClassTypeAdapter[T]) => ta
      case None                              => throw new ViewException(s"Output of view() must be a case class.  ${tt.tpe.typeSymbol.fullName} is not a case class.")
    }
    val masterData = master.getClass.getDeclaredFields
    val args = viewTarget.fieldMembers.map { f =>
      masterData.find(md => md.getName == f.name && md.getType == f.asInstanceOf[FieldMember[_, _]].valueAccessorMethod.getReturnType).map(dataField => {
        dataField.setAccessible(true)
        dataField.get(master)
      })
    }.flatten.toList
    viewTarget.constructorMirror.apply(args: _*).asInstanceOf[T]
  }

  /**
   * Splice a view (subset) object's fields into a master object's fields.
   * @param view the subset object
   * @param master master object
   * @return the master object with the view object's corresponding fields merged/overlayed
   */
  def spliceInto[T, U](view: T, master: U)(implicit tu: TypeTag[U]): U = {
    val masterTarget = context.typeAdapter(tu.tpe).maybeAs[CaseClassTypeAdapter[T]] match {
      case Some(ta: CaseClassTypeAdapter[T]) => ta
      case None                              => throw new ViewException(s"Output of view() must be a case class.  ${tu.tpe.typeSymbol.fullName} is not a case class.")
    }
    val viewData = view.getClass.getDeclaredFields
    val masterData = master.getClass.getDeclaredFields
    val args = masterTarget.fieldMembers.map { f =>
      viewData.find(vd => vd.getName == f.name && vd.getType == f.asInstanceOf[FieldMember[_, _]].valueAccessorMethod.getReturnType).map(dataField => {
        // Found matching master field in view object
        dataField.setAccessible(true)
        dataField.get(view)
      }).getOrElse(masterData.find(_.getName == f.name).map { dataField =>
        // Didn't find matching master field in view object--just use original field from master object
        dataField.setAccessible(true)
        dataField.get(master)
      }.get)
    }
    masterTarget.constructorMirror.apply(args: _*).asInstanceOf[U]
  }

  protected def bakeContext(): Context = {

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
              new TraitDeserializer[T](hintLabel, typeTypeAdapter.deserializer, Some(hintToType.memoized)),
              new TraitSerializer[T](hintLabel, typeTypeAdapter.serializer, Some(hintToType.memoized)),
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
            new TypeDeserializer(mod.apply),
            new TypeSerializer(mod.unapply),
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
      Some(this)
    )

    // ParseOrElse functionality
    val parseOrElseFactories = parseOrElseMap.map {
      case (attemptedType, fallbackType) =>
        val attemptedTypeAdapter = intermediateContext.typeAdapter(attemptedType)
        val fallbackTypeAdapter = intermediateContext.typeAdapter(fallbackType)

        new TypeAdapterFactory {
          override def typeAdapterOf[T](next: TypeAdapterFactory)(implicit context: Context, typeTag: TypeTag[T]): TypeAdapter[T] =
            if (typeTag.tpe =:= attemptedType) {
              val primary = attemptedTypeAdapter.asInstanceOf[TypeAdapter[T]]
              val secondary = fallbackTypeAdapter.asInstanceOf[TypeAdapter[T]]
              FallbackTypeAdapter[T](
                new FallbackDeserializer[T](primary.deserializer, secondary.deserializer),
                primary.serializer,
                primary, secondary)
            } else {
              next.typeAdapterOf[T]
            }
        }
    }.toList

    intermediateContext.copy(
      factories = parseOrElseFactories ::: intermediateContext.factories)
  }
}

case class ViewException(msg: String) extends Exception(msg)
