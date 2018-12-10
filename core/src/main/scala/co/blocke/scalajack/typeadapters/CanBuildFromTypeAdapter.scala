package co.blocke.scalajack
package typeadapters

import model._
import util.{ Reflection, Path }
import scala.collection.generic.CanBuildFrom
import scala.collection.{ GenMapLike, GenTraversableOnce, mutable }

object CanBuildFromTypeAdapterFactory extends TypeAdapterFactory.<:<.withOneTypeParam[GenTraversableOnce] {

  override def create[E, T <: GenTraversableOnce[E]](next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T], ttX: TypeTag[GenTraversableOnce[E]], ttElement: TypeTag[E]): TypeAdapter[T] = {
    val requiredClassSymbol = tt.tpe.typeSymbol.asClass

    val companionSymbol = requiredClassSymbol.companion.asModule
    val companionType = companionSymbol.info

    // Examples in comments reference Scala's List[A] type.

    val methods = for (member <- companionType.members if member.isMethod) yield member.asMethod

    // `implicit def canBuildFrom[A]: CanBuildFrom[Coll, A, List[A]] = ...`
    val implicitConversions = for (method <- methods if method.isImplicit && method.paramLists.flatten.isEmpty && method.returnType <:< typeOf[CanBuildFrom[_, _, _]]) yield method

    val matchingTypeAdapters = implicitConversions flatMap { method =>
      val returnTypeAsCanBuildFrom = method.returnType.baseType(typeOf[CanBuildFrom[_, _, _]].typeSymbol)

      // typeParam == A
      //      val typeParams = method.typeParams

      // toType == List[A]
      val toType = returnTypeAsCanBuildFrom.typeArgs(2)

      val typeParamSubstitutions: List[(Symbol, Type)] = method.typeParams.flatMap { typeParam =>
        // typeParam == A
        // optionalTypeArg == Some(String)
        val optionalTypeArg = Reflection.solveForNeedleAfterSubstitution(
          haystackBeforeSubstitution = toType,
          haystackAfterSubstitution  = tt.tpe.baseType(toType.typeSymbol),
          needleBeforeSubstitution   = typeParam.asType.toType)
        optionalTypeArg.map(typeArg => typeParam -> typeArg)
      }

      // elementTypeBeforeSubstitution == A
      val elementTypeBeforeSubstitution = returnTypeAsCanBuildFrom.typeArgs(1)

      // elementTypeAfterSubstitution == String
      val elementTypeAfterSubstitution = elementTypeBeforeSubstitution.substituteTypes(typeParamSubstitutions.map(_._1), typeParamSubstitutions.map(_._2))

      // elementTypeAdapter == TypeAdapter[String]
      val elementTypeAdapter = context.typeAdapter(elementTypeAfterSubstitution)

      val companionInstance = reflectModule(companionSymbol).instance
      val canBuildFrom = reflect(companionInstance).reflectMethod(method).apply()

      if (tt.tpe <:< typeOf[GenMapLike[_, _, _]] && elementTypeAfterSubstitution <:< typeOf[(_, _)]) {
        // $COVERAGE-OFF$Not sure how to trigger this! Here for extra safety, really.
        throw new java.lang.UnsupportedOperationException("This functionality has not yet been implemented")
        // $COVERAGE-ON$
      } else {
        def newBuilder(): mutable.Builder[E, T] = canBuildFrom.asInstanceOf[CanBuildFrom[Any, E, T]]()

        /*
        Some(CanBuildFromTypeAdapter[E, T](
          new CollectionIRTransceiver[E, T](elementTypeAdapter.irTransceiver.asInstanceOf[IRTransceiver[E]], () => newBuilder),
          canBuildFrom.asInstanceOf[CanBuildFrom[_, E, T]],
          elementTypeAdapter.asInstanceOf[TypeAdapter[E]]))
          */
        Some(CanBuildFromTypeAdapter[E, T](newBuilder, context.flavor.getArrayParser[E]()))
      }
    }

    matchingTypeAdapters.headOption.map(_.asInstanceOf[TypeAdapter[T]]).getOrElse(next.typeAdapterOf[T])
  }

}

//case class CanBuildMapTypeAdapter[Key, Value, To <: GenMapLike[Key, Value, To]](
//  override val irTransceiver: IRTransceiver[To],
//  canBuildFrom:               CanBuildFrom[_, (Key, Value), To],
//  keyTypeAdapter:             TypeAdapter[Key],
//  valueTypeAdapter:           TypeAdapter[Value]) extends TypeAdapter[To]

//case class CanBuildFromTypeAdapter[Elem, To <: GenTraversableOnce[Elem]](
//  override val irTransceiver: IRTransceiver[To],
//  canBuildFrom:               CanBuildFrom[_, Elem, To],
//  elementTypeAdapter:         TypeAdapter[Elem])(implicit tt: TypeTag[To]) extends TypeAdapter[To]

case class CanBuildFromTypeAdapter[Elem, To <: GenTraversableOnce[Elem]](elementsBuilder: mutable.Builder[Elem, To], parser: ArrayParser[Elem]) extends TypeAdapter[To] {

  override def materialize(primitive: AST_PRIMITIVE): To = (primitive match {
    case k: List[_] =>
      k.foreach(e => elementsBuilder += parser.elementTypeAdapter.materialize(e))
      elementsBuilder.result

    case null => null
    case _    => throw new Exception("Boom Array")
  }).asInstanceOf[To]

  override def dematerialize(t: To): AST_PRIMITIVE = t.seq.map(e => parser.elementTypeAdapter.dematerialize(e))
}
