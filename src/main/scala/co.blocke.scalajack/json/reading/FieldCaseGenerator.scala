package co.blocke.scalajack
package json
package reading

import scala.quoted.*
import co.blocke.scala_reflection.reflect.rtypeRefs.*
import co.blocke.scala_reflection.RTypeRef
import shared.CodecBuildContext

object FieldCaseGenerator:

  def generateConstructorFieldCases(
      ctx: CodecBuildContext,
      cfg: SJConfig,
      classRef: ScalaClassRef[?],
      reqSym: ctx.quotes.reflect.Symbol,
      fieldSymbols: Map[Int, ctx.quotes.reflect.Symbol],
      in: Expr[JsonSource]
  ): List[ctx.quotes.reflect.CaseDef] =
    given Quotes = ctx.quotes
    import ctx.quotes.reflect.* // { Symbol as RSymbol, *, given }

    classRef.fields.map { field =>
      field.fieldRef.refType match
        case '[f] =>
          val reqBit = Expr(1L << field.index)
          val fieldName = Expr(field.name)
          val varSym = fieldSymbols(field.index)
          val fieldRef = Ref(varSym)

          val caseBody = field.fieldRef match {
            case _: OptionRef[?] | _: AnyRef =>
              Assign(fieldRef, Reader.genReadVal[f](ctx, cfg, field.fieldRef.asInstanceOf[RTypeRef[f]], in).asTerm)
                .asExprOf[Unit]
                .asTerm

            case a: AliasRef[?] if a.unwrappedType.isInstanceOf[OptionRef[?]] =>
              Assign(fieldRef, Reader.genReadVal[f](ctx, cfg, field.fieldRef.asInstanceOf[RTypeRef[f]], in).asTerm)
                .asExprOf[Unit]
                .asTerm

            case t: LeftRightRef[?] if t.hasOptionChild.isDefined =>
              Assign(fieldRef, Reader.genReadVal[f](ctx, cfg, field.fieldRef.asInstanceOf[RTypeRef[f]], in).asTerm).asExprOf[Unit].asTerm

            case t: TryRef[?] if t.hasOptionChild.isDefined =>
              Assign(fieldRef, Reader.genReadVal[f](ctx, cfg, field.fieldRef.asInstanceOf[RTypeRef[f]], in).asTerm).asExprOf[Unit].asTerm
            case _ =>
              '{
                if (${ Ref(reqSym).asExprOf[Long] } & $reqBit) != 0 then
                  ${ Assign(Ref(reqSym), '{ ${ Ref(reqSym).asExprOf[Long] } ^ $reqBit }.asTerm).asExprOf[Unit] }
                  ${ Assign(fieldRef, Reader.genReadVal[f](ctx, cfg, field.fieldRef.asInstanceOf[RTypeRef[f]], in).asTerm).asExprOf[Unit] }
                else throw new JsonParseError("Duplicate field " + $fieldName, $in)
              }.asTerm
          }

          CaseDef(
            Literal(IntConstant(field.index)),
            None,
            caseBody
          )
    }
