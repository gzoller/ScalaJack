package co.blocke.scalajack

import json.JsonFlavor
import typeadapter.{ FallbackTypeAdapter, PolymorphicTypeAdapter, PolymorphicTypeAdapterFactory, CaseClassTypeAdapter }
import BijectiveFunction.Implicits._
import BijectiveFunctions._

import scala.language.existentials
import scala.reflect.runtime.universe.{ Type, TypeTag }

object ScalaJack {
  def apply[S](kind: ScalaJackLike[S] = JsonFlavor()): ScalaJackLike[S] = kind
}

abstract class ScalaJackLike[S] extends JackFlavor[S] {
  val customAdapters: List[TypeAdapterFactory]
  val hintMap: Map[Type, String]
  val hintModifiers: Map[Type, HintModifier]
  val parseOrElseMap: Map[Type, Type]
  val defaultHint: String
  val isCanonical: Boolean

  val context: Context = bakeContext()

  def withAdapters(ta: TypeAdapterFactory*): ScalaJackLike[S]
  def withHints(h: (Type, String)*): ScalaJackLike[S]
  def withHintModifiers(hm: (Type, HintModifier)*): ScalaJackLike[S]
  def withDefaultHint(hint: String): ScalaJackLike[S]
  def parseOrElse(poe: (Type, Type)*): ScalaJackLike[S]
  def isCanonical(canonical: Boolean): ScalaJackLike[S]

  /**
   * Project fields from given master object to a view object of type T.  Field names/types must match master
   * precisely.
   * @param master the master object from which the smaller object is projected
   * @return an object of type T which is a "subset" of the master
   */
  // WARNING: Assumes CaseClassTypeAdapter.members is in constructor-order.  If not, sort on members.index.
  def view[T](master: Any)(implicit tt: TypeTag[T]): T = {
    val viewTarget = context.typeAdapter(tt.tpe) match {
      case ta: CaseClassTypeAdapter[_] => ta
      case _                           => throw new ViewException(s"Output of view() must be a case class.  ${tt.tpe.typeSymbol.fullName} is not a case class.")
    }
    val masterData = master.getClass.getDeclaredFields
    val args = viewTarget.members.map { f =>
      masterData.find(md => md.getName == f.name && md.getType == f.valueAccessorMethod.getReturnType).map(dataField => {
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
    val masterTarget = context.typeAdapter(tu.tpe) match {
      case ta: CaseClassTypeAdapter[_] => ta
      case _                           => throw new ViewException(s"Output of spliceInto() must be a case class.  ${tu.tpe.typeSymbol.fullName} is not a case class.")
    }
    val viewData = view.getClass.getDeclaredFields
    val masterData = master.getClass.getDeclaredFields
    val args = masterTarget.members.map { f =>
      viewData.find(vd => vd.getName == f.name && vd.getType == f.valueAccessorMethod.getReturnType).map(dataField => {
        // Found matching master field in view object
        dataField.setAccessible(true)
        dataField.get(view)
      }).getOrElse(masterData.find(_.getName == f.name).map { dataField =>
        // Didn't find matching master field in view object--just use original field from master object
        dataField.setAccessible(true)
        dataField.get(master)
      }.get)
    }.toList
    masterTarget.constructorMirror.apply(args: _*).asInstanceOf[U]
  }

  protected def bakeContext(): Context = {

    val polymorphicTypes: Set[Type] = hintModifiers.keySet ++ hintMap.keySet

    val polymorphicTypeAdapterFactories = polymorphicTypes.map { polymorphicType: Type ⇒
      val hintFieldName = hintMap.getOrElse(polymorphicType, defaultHint)
      val hintToType = hintModifiers.getOrElse(polymorphicType, fullNameToType)

      new TypeAdapterFactory {
        override def typeAdapter(tpe: Type, context: Context): Option[TypeAdapter[_]] = {
          if (tpe.typeSymbol == polymorphicType.typeSymbol) {
            val stringTypeAdapter = context.typeAdapterOf[String]
            Some(PolymorphicTypeAdapter(hintFieldName, stringTypeAdapter andThen hintToType.memoized, context.typeAdapterOf[MemberName], context, tpe))
          } else {
            None
          }
        }
      }
    }.toList

    val intermediateContext = Context(
      defaultHint,
      factories = customAdapters ::: polymorphicTypeAdapterFactories ::: Context.StandardContext.factories ::: List(PolymorphicTypeAdapterFactory(defaultHint))
    )

    // ParseOrElse functionality
    val fallbackFactories = parseOrElseMap.map {
      case (attemptedType, fallbackType) ⇒
        val attemptedTypeAdapter = intermediateContext.typeAdapter(attemptedType)
        val fallbackTypeAdapter = intermediateContext.typeAdapter(fallbackType)

        new TypeAdapterFactory {
          override def typeAdapter(tpe: Type, context: Context): Option[TypeAdapter[_]] =
            if (tpe =:= attemptedType) {
              Some(FallbackTypeAdapter[Any](attemptedTypeAdapter.asInstanceOf[TypeAdapter[Any]], fallbackTypeAdapter.asInstanceOf[TypeAdapter[Any]]))
            } else {
              None
            }
        }
    }.toList

    intermediateContext.copy(
      factories = fallbackFactories ::: intermediateContext.factories
    )
  }
}

case class ViewException(msg: String) extends Exception(msg)
