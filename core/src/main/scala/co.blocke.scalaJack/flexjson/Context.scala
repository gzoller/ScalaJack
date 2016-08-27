package co.blocke.scalajack.flexjson

import co.blocke.scalajack.flexjson.typeadapter.{CaseClassTypeAdapter, ListTypeAdapter, LongTypeAdapter, OptionTypeAdapter, StringTypeAdapter}

import scala.reflect.runtime.universe.{Type, TypeTag}

object Context {

  val StandardContext = Context()
    .withFactory(ListTypeAdapter)
    .withFactory(CaseClassTypeAdapter)
    .withFactory(OptionTypeAdapter)
    .withFactory(LongTypeAdapter)
    .withFactory(StringTypeAdapter)

}

case class Context(factories: List[TypeAdapterFactory] = Nil) {

  var resolvedTypeAdapters = Map[Type, TypeAdapter[_]]()

  def withFactory(factory: TypeAdapterFactory): Context =
    copy(factories = factories :+ factory)

  def typeAdapter(tpe: Type): TypeAdapter[_] =
    resolvedTypeAdapters.get(tpe) match {
      case Some(typeAdapter) ⇒
        typeAdapter

      case None ⇒
        var optionalTypeAdapter: Option[TypeAdapter[_]] = None

        var remainingFactories = factories
        while (optionalTypeAdapter.isEmpty && remainingFactories.nonEmpty) {
          optionalTypeAdapter = remainingFactories.head.typeAdapter(tpe, this)
          remainingFactories = remainingFactories.tail
        }

        val typeAdapter = optionalTypeAdapter.getOrElse(throw new IllegalArgumentException(s"Cannot find a type adapter for $tpe"))

        resolvedTypeAdapters += tpe → typeAdapter

        typeAdapter
    }

  def typeAdapterOf[T](implicit valueTypeTag: TypeTag[T]): TypeAdapter[T] =
    typeAdapter(valueTypeTag.tpe).asInstanceOf[TypeAdapter[T]]

}
