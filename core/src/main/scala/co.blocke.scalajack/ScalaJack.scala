package co.blocke.scalajack

import json.JsonFlavor
import typeadapter.{ FallbackTypeAdapter, PolymorphicTypeAdapter, PolymorphicTypeAdapterFactory }
import BijectiveFunction.Implicits._
import BijectiveFunctions._

import scala.reflect.runtime.universe.Type

object ScalaJack {
  def apply[S](kind: JackFlavor[S] with ScalaJackLike[S] = JsonFlavor()): JackFlavor[S] with ScalaJackLike[S] = kind
}

trait ScalaJackLike[S] {
  self: JackFlavor[S] => // require a JackFlavor to be mixed in

  private var customAdapters = List.empty[TypeAdapterFactory]
  private var hintMap = Map.empty[Type, String]
  private var hintModifiers = Map.empty[Type, BijectiveFunction[String, Type]]
  private var parseOrElseMap = Map.empty[Type, Type]
  private var defaultHint = "_hint"
  private var _context: Context = bakeContext()

  def context = _context

  private def rebuild() = {
    _context = bakeContext()
    this
  }

  def withAdapters(ta: TypeAdapterFactory*) = {
    customAdapters = customAdapters ++ ta.toList
    rebuild()
  }
  def withHints(h: (Type, String)*) = {
    hintMap = hintMap ++ h
    rebuild()
  }
  def withHintModifiers(hm: (Type, BijectiveFunction[String, Type])*) = {
    hintModifiers = hintModifiers ++ hm
    rebuild()
  }
  def withDefaultHint(hint: String) = {
    defaultHint = hint
    rebuild()
  }
  def parseOrElse(poe: (Type, Type)*) = {
    parseOrElseMap = parseOrElseMap ++ poe
    rebuild()
  }

  //----------
  //  private val contextCache = new mutable.WeakHashMap[VisitorContext, Context]
  // val context = Context.StandardContext

  private def bakeContext(): Context = {

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

//-----------------------------------------------------------

// MOVE TO UTIL CLASS OR SOMETHING!
// object JSON {
// 	def toCollection( js:String, size:Int = 500 ) : Either[Map[String,Any],List[Any]] = json.FastTokenizer(size).tokenize(js.toCharArray).toCollection()
// }
