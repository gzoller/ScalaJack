package co.blocke.scalajack.shared

import co.blocke.scala_reflection.RTypeRef
import co.blocke.scala_reflection.reflect.ReflectOnType
import co.blocke.scala_reflection.reflect.rtypeRefs.AliasRef
import scala.quoted.*

/** scala-reflection recognizes simple opaque aliases, but classifies applied opaque
  * aliases as classes. Resolve their representation before generating class codecs.
  */
object OpaqueSupport:
  def normalize[T: Type](ctx: CodecBuildContext, ref: RTypeRef[T]): RTypeRef[T] =
    given Quotes = ctx.quotes
    import ctx.quotes.reflect.*

    TypeRepr.of[T].dealias match
      case applied @ AppliedType(alias: TypeRef, args) if alias.isOpaqueAlias =>
        val underlying = alias.translucentSuperType.appliedTo(args).dealias
        underlying.asType match
          case '[u] =>
            val unwrapped = ReflectOnType[u](ctx.quotes)(underlying)(using ctx.seenBefore)
            AliasRef[T](applied.show, normalize[u](ctx, unwrapped))
      case _ => ref
