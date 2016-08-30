package co.blocke.scalajack.flexjson

import co.blocke.scalajack.flexjson.typeadapter.javaprimitives.{JavaBooleanTypeAdapter, JavaByteTypeAdapter, JavaCharacterTypeAdapter, JavaDoubleTypeAdapter, JavaFloatTypeAdapter, JavaIntegerTypeAdapter, JavaLongTypeAdapter, JavaShortTypeAdapter}
import co.blocke.scalajack.flexjson.typeadapter.joda.JodaDateTimeTypeAdapter
import co.blocke.scalajack.flexjson.typeadapter.{BooleanTypeAdapter, ByteTypeAdapter, CaseClassTypeAdapter, CharTypeAdapter, DerivedValueClassAdapter, DoubleTypeAdapter, EnumerationTypeAdapter, FloatTypeAdapter, IntTypeAdapter, ListTypeAdapter, LongTypeAdapter, MapTypeAdapter, OptionTypeAdapter, SetTypeAdapter, ShortTypeAdapter, StringTypeAdapter, TryTypeAdapter, TypeTypeAdapter, UUIDTypeAdapter}

import scala.reflect.runtime.universe.{Type, TypeTag}

object Context {

  val StandardContext = Context()
    .withFactory(TypeTypeAdapter)
    .withFactory(ListTypeAdapter)
    .withFactory(SetTypeAdapter)
    .withFactory(MapTypeAdapter)
    .withFactory(CaseClassTypeAdapter)
    .withFactory(OptionTypeAdapter)
    .withFactory(TryTypeAdapter)
    .withFactory(BooleanTypeAdapter)
    .withFactory(CharTypeAdapter)
    .withFactory(ByteTypeAdapter)
    .withFactory(ShortTypeAdapter)
    .withFactory(IntTypeAdapter)
    .withFactory(LongTypeAdapter)
    .withFactory(FloatTypeAdapter)
    .withFactory(DoubleTypeAdapter)
    .withFactory(StringTypeAdapter)
    .withFactory(DerivedValueClassAdapter)
    .withFactory(EnumerationTypeAdapter)
// FIXME    .withFactory(PolymorphicTypeAdapter)
    .withFactory(JavaBooleanTypeAdapter)
    .withFactory(JavaByteTypeAdapter)
    .withFactory(JavaCharacterTypeAdapter)
    .withFactory(JavaDoubleTypeAdapter)
    .withFactory(JavaFloatTypeAdapter)
    .withFactory(JavaIntegerTypeAdapter)
    .withFactory(JavaLongTypeAdapter)
    .withFactory(JavaShortTypeAdapter)
    .withFactory(UUIDTypeAdapter)
    .withFactory(JodaDateTimeTypeAdapter)

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
        val optionalTypeAdapter:Option[TypeAdapter[_]] = factories.find( f => f.typeAdapter(tpe,this).isDefined ).flatMap( f => f.typeAdapter(tpe,this) )
        val typeAdapter = optionalTypeAdapter.getOrElse(throw new IllegalArgumentException(s"Cannot find a type adapter for $tpe"))

        resolvedTypeAdapters += tpe → LazyTypeAdapter(this, tpe)
        resolvedTypeAdapters += tpe → typeAdapter

        typeAdapter
    }

  def typeAdapterOf[T](implicit valueTypeTag: TypeTag[T]): TypeAdapter[T] =
    typeAdapter(valueTypeTag.tpe).asInstanceOf[TypeAdapter[T]]

}
