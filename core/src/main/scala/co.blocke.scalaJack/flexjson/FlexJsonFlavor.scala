package co.blocke.scalajack.flexjson

import co.blocke.scalajack.flexjson.typeadapter.PolymorphicTypeAdapter
import co.blocke.scalajack.{FlavorKind, JackFlavor, ScalaJack, VisitorContext}

import scala.collection.mutable
import scala.reflect.runtime.universe.{Type, TypeTag}

object FlexJsonFlavor extends FlavorKind[String] with ScalaJack[String] with JackFlavor[String] {

  override val makeScalaJack = this

  override val rr = FlexReadRenderer

  object FlexReadRenderer extends ReadRenderer {

    val context = Context.StandardContext

    val contextCache = new mutable.WeakHashMap[VisitorContext, Context]

    def context(vc: VisitorContext): Context =
      contextCache.getOrElseUpdate(vc, {
        import BijectiveFunction.Implicits._
        import BijectiveFunctions._

        val polymorphicFullNames: Set[String] = vc.hintValueRead.keySet ++ vc.hintValueRender.keySet ++ vc.hintMap.keySet.filter(_ != "default")

        val defaultHintFieldName: String = vc.hintMap.getOrElse("default", "_hint")

        val polymorphicTypeAdapterFactories = polymorphicFullNames map { polymorphicFullName ⇒
          val polymorphicType = fullNameToType(polymorphicFullName)

          val hintFieldName = vc.hintMap.getOrElse(polymorphicFullName, defaultHintFieldName)

          val hintToType: BijectiveFunction[String, Type] = {
            val optionalCustomApply: Option[String ⇒ Type] = vc.hintValueRead.get(polymorphicFullName).map(f ⇒ f andThen fullNameToType)
            val optionalCustomUnapply: Option[Type ⇒ String] = vc.hintValueRender.get(polymorphicFullName).map(f ⇒ typeToFullName andThen f)

            if (optionalCustomApply.isDefined || optionalCustomUnapply.isDefined) {
              val customApply: (String ⇒ Type) = optionalCustomApply.getOrElse(_ ⇒ throw new Exception(s"""Cannot serialize ${typeToFullName(polymorphicType)} because the visitor context's hintValueReader lacks an entry whose key is "$polymorphicFullName""""))
              val customUnapply: (Type ⇒ String) = optionalCustomUnapply.getOrElse(_ ⇒ throw new Exception(s"""Cannot deserialize ${typeToFullName(polymorphicType)} because the visitor context's hintValueRead lacks an entry whose key is "$polymorphicFullName""""))

              customApply ⇄ customUnapply
            } else {
              fullNameToType
            }
          }

          val polymorphicTypeAdapterFactory = new TypeAdapterFactory {

            override def typeAdapter(tpe: Type, context: Context): Option[TypeAdapter[_]] =
// FIXME              if (tpe =:= polymorphicType) {
              if (tpe.typeSymbol.fullName == polymorphicFullName) {
                val stringTypeAdapter = context.typeAdapterOf[String]

                Some(PolymorphicTypeAdapter(hintFieldName, stringTypeAdapter andThen hintToType.memoized, context))
              } else {
                None
              }

          }

          polymorphicTypeAdapterFactory
        }

        val initialContext = this.context

        val finalContext = polymorphicTypeAdapterFactories.foldLeft(initialContext)((intermediateContext, factory) ⇒ intermediateContext withFactory factory) withFactory PolymorphicTypeAdapter

        finalContext
      })

    override def read[T](json: String)(implicit valueTypeTag: TypeTag[T], vc: VisitorContext): T = {
      val tokenizer = new Tokenizer

      val source = json.toCharArray
      val reader = tokenizer.tokenize(source, 0, source.length)

      context(vc).typeAdapterOf[T].read(reader)
    }

    override def render[T](value: T)(implicit valueTypeTag: TypeTag[T], context: VisitorContext): String = ???

  }

}
