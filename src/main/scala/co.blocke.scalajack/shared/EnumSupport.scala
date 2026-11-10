package co.blocke.scalajack.shared

import co.blocke.scala_reflection.{RTypeRef, string2typedname, extractAnnotationInfo}
import co.blocke.scala_reflection.reflect.ReflectOnType
import co.blocke.scala_reflection.reflect.rtypeRefs.*
import scala.quoted.*

/** Adapt payload enums to the same representation used by sealed class hierarchies.
  * scala-reflection currently represents all Scala enums as lists of case names.
  */
object EnumSupport:
  def hasPayload(ref: ScalaEnumRef[?])(using Quotes): Boolean =
    import quotes.reflect.*
    ref.refType match
      case '[e] =>
        val symbol = TypeRepr.of[e].typeSymbol
        symbol.children.exists(_.isClassDef) || (symbol.isClassDef && symbol.flags.is(Flags.Case))

  def hierarchy[T: Type](ctx: CodecBuildContext, ref: ScalaEnumRef[?]): RTypeRef[T] =
    given Quotes = ctx.quotes
    import ctx.quotes.reflect.*

    def caseRef[C: Type](tpe: TypeRepr): ScalaClassRef[C] =
      val symbol = tpe.typeSymbol
      val singleton = tpe match
        case _: TermRef => true
        case _ => false
      val name = if singleton then tpe.termSymbol.fullName else symbol.fullName
      val fields = (if singleton then Nil else symbol.caseFields).zipWithIndex.map { (field, index) =>
        val fieldRef = tpe.memberType(field).asType match
          case '[f] => ReflectOnType[f](ctx.quotes)(tpe.memberType(field))(using ctx.seenBefore)
        ScalaFieldInfoRef(index, field.name, fieldRef, extractAnnotationInfo(ctx.quotes)(field.annotations), None, None)
      }
      ScalaClassRef[C](
        name, name, Nil, Nil, Nil, fields, extractAnnotationInfo(ctx.quotes)(symbol.annotations), Nil,
        isAppliedType = false, isValueClass = false, isCaseClass = true,
        isAbstractClass = false, typePaths = Map.empty
      )

    val tpe = TypeRepr.of[T]
    if tpe.typeSymbol.children.isEmpty then caseRef[T](tpe)
    else
      val children = tpe.typeSymbol.children.map { child =>
        val childType = if child.isTerm then child.termRef else child.typeRef
        childType.asType match
          case '[c] => caseRef[c](childType)
      }
      TraitRef[T](ref.name, ref.typedName, Nil, annotations = Map.empty, sealedChildren = children)
