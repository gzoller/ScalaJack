package co.blocke.scalajack.flexjson

import typeadapter._

import scala.reflect.runtime.universe.{ Type, TypeTag }

object Context {

  val StandardContext = Context()
    .withFactory(TypeTypeAdapter)
    .withFactory(CaseClassTypeAdapter)
    .withFactory(IntTypeAdapter)
    .withFactory(StringTypeAdapter)
}

case class Context(factories: List[TypeAdapterFactory] = Nil) {

  var resolvedTypeAdapters = Map[Type, TypeAdapter[_]]()

  def withFactory(factory: TypeAdapterFactory): Context =
    copy(factories = factories :+ factory)

  def typeAdapter(tpe: Type, superParamTypes: List[Type] = List.empty[Type]): TypeAdapter[_] = {
    val tsym = tpe.typeSymbol.asType.typeParams
    val args = tpe.typeArgs

    resolvedTypeAdapters.get(tpe) match {
      case Some(typeAdapter) ⇒
        typeAdapter

      case None ⇒
        resolvedTypeAdapters += tpe → LazyTypeAdapter(this, tpe)

        var optionalTypeAdapter: Option[TypeAdapter[_]] = None

        var remainingFactories = factories
        while (optionalTypeAdapter.isEmpty && remainingFactories.nonEmpty) {
          optionalTypeAdapter = remainingFactories.head.typeAdapter(tpe, this, superParamTypes)
          remainingFactories = remainingFactories.tail
        }

        val typeAdapter = optionalTypeAdapter.getOrElse(throw new IllegalArgumentException(s"Cannot find a type adapter for $tpe"))

        resolvedTypeAdapters += tpe → typeAdapter

        typeAdapter
    }
  }

  def typeAdapterOf[T](implicit valueTypeTag: TypeTag[T]): TypeAdapter[T] =
    typeAdapter(valueTypeTag.tpe).asInstanceOf[TypeAdapter[T]]

}
