package co.blocke.scalajack
package xml
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
      in: Expr[XmlSource]
  ): List[ctx.quotes.reflect.CaseDef] =
    given Quotes = ctx.quotes
    import ctx.quotes.reflect.* // { Symbol as RSymbol, *, given }

    classRef.fields.map { field =>
      field.fieldRef.refType match
        case '[f] =>
          val reqBit = Expr(1 << field.index)
          val fieldName = Expr(field.name)
          val varSym = fieldSymbols(field.index)
          val fieldRef = Ref(varSym)
          val entryLabel = field.annotations
            .get("co.blocke.scalajack.xmlEntryLabel")
            .flatMap(_.get("name"))

          val caseBody = field.fieldRef match {
            case _: OptionRef[?] | _: AnyRef =>
              Assign(fieldRef, Reader.genReadVal[f](ctx, cfg, field.fieldRef.asInstanceOf[RTypeRef[f]], in, false, false, field.name, entryLabel).asTerm).asExprOf[Unit].asTerm

            case t: LeftRightRef[?] if t.hasOptionChild.isDefined =>
              Assign(fieldRef, Reader.genReadVal[f](ctx, cfg, field.fieldRef.asInstanceOf[RTypeRef[f]], in, false, false, field.name, entryLabel).asTerm).asExprOf[Unit].asTerm

            case t: TryRef[?] if t.hasOptionChild.isDefined =>
              Assign(fieldRef, Reader.genReadVal[f](ctx, cfg, field.fieldRef.asInstanceOf[RTypeRef[f]], in, false, false, field.name, entryLabel).asTerm).asExprOf[Unit].asTerm
            case _ =>
              '{
                if (${ Ref(reqSym).asExprOf[Int] } & $reqBit) != 0 then
                  ${ Assign(Ref(reqSym), '{ ${ Ref(reqSym).asExprOf[Int] } ^ $reqBit }.asTerm).asExprOf[Unit] }
                  ${ Assign(fieldRef, Reader.genReadVal[f](ctx, cfg, field.fieldRef.asInstanceOf[RTypeRef[f]], in, false, false, field.name, entryLabel).asTerm).asExprOf[Unit] }
                else throw new ParseError("Duplicate field " + $fieldName)
              }.asTerm
          }

          CaseDef(
            Literal(IntConstant(field.index)),
            None,
            caseBody
          )
    }
