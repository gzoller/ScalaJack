package co.blocke.scalajack.json

import co.blocke.scalajack.json.typeadapter.{ FallbackTypeAdapter, PolymorphicTypeAdapter, PolymorphicTypeAdapterFactory }
import co.blocke.scalajack.{ FlavorKind, JackFlavor, ScalaJack, VisitorContext }

import scala.collection.mutable
import scala.reflect.runtime.universe.{ Type, TypeTag }
import scala.reflect.runtime.currentMirror

object JsonFlavor extends FlavorKind[String] with ScalaJack[String] with JackFlavor[String] {

  type MemberName = String

  override val makeScalaJack = this

  override val rr = FlexReadRenderer

  object FlexReadRenderer extends ReadRenderer {

    val context = Context.StandardContext

    val contextCache = new mutable.WeakHashMap[VisitorContext, Context]

    def context(vc: VisitorContext): Context =
      contextCache.getOrElseUpdate(vc, {
        import BijectiveFunction.Implicits._
        import BijectiveFunctions._

        val polymorphicFullNames: Set[String] = Set() ++
          vc.hintValueRead.keySet ++
          vc.hintValueRender.keySet ++
          vc.hintMap.keySet.filter(_ != "default")

        val defaultHintFieldName: String = vc.hintMap.getOrElse("default", "_hint")

        val customHandlerTypeAdapterFactories = vc.customAdapters

        val polymorphicTypeAdapterFactories = polymorphicFullNames map { polymorphicFullName ⇒
          val polymorphicType = fullNameToType(polymorphicFullName)

          val hintFieldName = vc.hintMap.getOrElse(polymorphicFullName, defaultHintFieldName)

          val hintToType: BijectiveFunction[String, Type] = {
            val optionalCustomApply: Option[String ⇒ Type] = vc.hintValueRead.get(polymorphicFullName).map(f ⇒ f andThen fullNameToType)
            val optionalCustomUnapply: Option[Type ⇒ String] = vc.hintValueRender.get(polymorphicFullName).map(f ⇒ typeToFullName andThen f)

            if (optionalCustomApply.isDefined || optionalCustomUnapply.isDefined) {
              val customApply: (String ⇒ Type) = optionalCustomApply.getOrElse(_ ⇒ throw new Exception(s"""Cannot serialize ${typeToFullName(polymorphicType)} because the visitor context's hintValueReader lacks an entry whose key is "$polymorphicFullName""""))
              val customUnapply: (Type ⇒ String) = optionalCustomUnapply.getOrElse(_ ⇒ throw new Exception(s"""Cannot deserialize ${typeToFullName(polymorphicType)} because the visitor context's hintValueRender lacks an entry whose key is "$polymorphicFullName""""))

              customApply ⇄ customUnapply
            } else {
              fullNameToType
            }
          }

          val polymorphicTypeAdapterFactory = new TypeAdapterFactory {

            override def typeAdapter(tpe: Type, context: Context): Option[TypeAdapter[_]] = {
              // FIXME              if (tpe =:= polymorphicType) {
              //              if (tpe =:= polymorphicType) {
              if (tpe.typeSymbol == polymorphicType.typeSymbol) {
                // print("NAME: " + tpe.typeSymbol.fullName + "  -->  " + polymorphicFullName)
                // if (tpe.typeSymbol.fullName == polymorphicFullName) {
                val stringTypeAdapter = context.typeAdapterOf[String]

                Some(PolymorphicTypeAdapter(hintFieldName, stringTypeAdapter andThen hintToType.memoized, context.typeAdapterOf[MemberName], context, tpe))
              } else {
                None
              }
            }

          }

          polymorphicTypeAdapterFactory
        }

        val intermediateContext = context.copy(
          factories = customHandlerTypeAdapterFactories.toList ::: polymorphicTypeAdapterFactories.toList ::: context.factories ::: List(PolymorphicTypeAdapterFactory(defaultHintFieldName))
        )

        val fallbackFactories = vc.parseOrElse.map({
          case (attemptedFullName, fallbackFullName) ⇒
            val attemptedType = currentMirror.staticClass(attemptedFullName).asType.toType
            val attemptedTypeAdapter = intermediateContext.typeAdapter(attemptedType)

            val fallbackType = currentMirror.staticClass(fallbackFullName).asType.toType
            val fallbackTypeAdapter = intermediateContext.typeAdapter(fallbackType)

            new TypeAdapterFactory {
              override def typeAdapter(tpe: Type, context: Context): Option[TypeAdapter[_]] =
                if (tpe =:= attemptedType) {
                  Some(FallbackTypeAdapter[Any](attemptedTypeAdapter.asInstanceOf[TypeAdapter[Any]], fallbackTypeAdapter.asInstanceOf[TypeAdapter[Any]]))
                } else {
                  None
                }
            }
        })

        intermediateContext.copy(
          factories = fallbackFactories.toList ::: intermediateContext.factories
        )
      })

    override def read[T](json: String)(implicit valueTypeTag: TypeTag[T], visitorContext: VisitorContext): T = {
      val tokenizer = new Tokenizer

      val source = json.toCharArray
      val reader = tokenizer.tokenize(source, 0, source.length)

      val c = context(visitorContext)
      val typeAdapter = c.typeAdapterOf[T]
      typeAdapter.read(reader)
    }

    override def render[T](value: T)(implicit valueTypeTag: TypeTag[T], visitorContext: VisitorContext): String = {
      val writer = new StringJsonWriter(visitorContext.isCanonical)
      val typeAdapter = context(visitorContext).typeAdapterOf[T]
      typeAdapter.write(value, writer)
      val jsonString = writer.jsonString
      jsonString
    }

  }

}
