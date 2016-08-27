package co.blocke.scalajack.flexjson

import co.blocke.scalajack.{FlavorKind, JackFlavor, ScalaJack, VisitorContext}
import scala.reflect.runtime.universe.TypeTag

object FlexJsonFlavor extends FlavorKind[String] with ScalaJack[String] with JackFlavor[String] {

  override val makeScalaJack = this

  override val rr = FlexReadRenderer

  object FlexReadRenderer extends ReadRenderer {

    val context = Context.StandardContext

    override def read[T](json: String)(implicit valueTypeTag: TypeTag[T], visitorContext: VisitorContext): T = {
      val tokenizer = new Tokenizer

      val source = json.toCharArray
      val reader = tokenizer.tokenize(source, 0, source.length)

      context.typeAdapterOf[T].read(reader)
    }

    override def render[T](value: T)(implicit valueTypeTag: TypeTag[T], context: VisitorContext): String = ???

  }

}
