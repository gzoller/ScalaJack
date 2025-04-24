package co.blocke.scalajack
package json
package writing

import scala.quoted.*
import co.blocke.scala_reflection.reflect.rtypeRefs.*
import co.blocke.scala_reflection.{RType, RTypeRef, TypedName}

object Writer:

  def genWriterFor[W](ctx: CodecBuildContext, cfg: SJConfig, in: Expr[W], ref: RTypeRef[W], out: Expr[JsonOutput], isMapKey: Boolean = false)(using Type[W]): Expr[Unit] =
    given Quotes = ctx.quotes
    import ctx.quotes.reflect.*

    val typedName = ref.typedName

    // Phase 1: Register placeholder
    var realWriter: Expr[(Any, JsonOutput) => Unit] = '{ (_, _) => () }
    ctx.generatedWriters.getOrElseUpdate(typedName, realWriter)

    val body: Expr[Unit] = ref match
      case _: StringRef =>
        if cfg._suppressEscapedStrings then '{ $out.value($in) } else '{ $out.valueEscaped($in) }

      case _: IntRef =>
        if isMapKey then '{ $out.valueStringified($in) } else '{ $out.value($in) }

      case t: ScalaClassRef[?] =>
        ref.refType match
          case '[w] =>
            val typedIn = in.asExprOf[w]
            val begin = '{ $out.startObject() }
            val end = '{ $out.endObject() }

            val fieldWrites = t.fields.map { oneField =>
              oneField.fieldRef.refType match
                case '[f] =>
                  val fieldExpr = Select.unique(typedIn.asTerm, oneField.name).asExprOf[f]
                  val fieldNameExpr = Expr(oneField.name)
                  val writeField = genWriterFor[f](
                    ctx,
                    cfg,
                    fieldExpr,
                    oneField.fieldRef.asInstanceOf[RTypeRef[f]],
                    out
                  )(using Type.of[f])
                  '{
                    $out.label($fieldNameExpr)
                    $writeField
                  }
            }

            val bodyExpr = Expr.block(begin +: fieldWrites :+ end, '{ () })

            realWriter = '{
              (in: Any, out: JsonOutput) =>
                val typed = in.asInstanceOf[w]
                val outTyped = out
                ${ bodyExpr }
            }
            ctx.generatedWriters.update(typedName, realWriter)
            bodyExpr

      case t: Sealable if t.isSealed =>
        t.refType match
          case '[base] =>
            println("<<< SEALED >>>")
            val inTyped = in.asExprOf[base]

            val matchBranches = t.sealedChildren.collect {
              case sub: ScalaClassRef[?] =>
                sub.refType match
                  case '[subt] =>
                    val bindingSym = Symbol.newBind(Symbol.spliceOwner, "matched", Flags.EmptyFlags, TypeTree.of[subt].tpe)
                    val casePattern = Bind(bindingSym, Typed(Wildcard(), TypeTree.of[subt]))

                    val caseBody = {
                      val matchedExpr = Ref(bindingSym).asExprOf[subt]
                      genWriterFor[subt](ctx, cfg, matchedExpr, sub.asInstanceOf[RTypeRef[subt]], out)(using Type.of[subt])
                    }

                    CaseDef(casePattern, None, caseBody.asTerm)
            }

            Match(inTyped.asTerm, matchBranches).asExprOf[Unit]

      case t: SeqRef[?] =>
        t.elementRef.refType match
          case '[e] =>
            val tin = if t.isMutable then in.asExprOf[scala.collection.mutable.Seq[e]] else in.asExprOf[Seq[e]]
            '{
              if $tin == null then $out.burpNull()
              else {
                $out.startArray()
                $tin.foreach { i =>
                  ${ genWriterFor[e](ctx, cfg, '{ i }, t.elementRef.asInstanceOf[RTypeRef[e]], out)(using Type.of[e]) }
                }
                $out.endArray()
              }
            }

      case s: SelfRefRef[?] =>
        println(s"[🌀] Attempting SelfRef writer for ${s.typedName}")
        ctx.generatedWriters.get(s.typedName) match
          case Some(writerExpr) =>
            val typed = writerExpr.asInstanceOf[Expr[(Any, JsonOutput) => Unit]]
            '{ $typed($in.asInstanceOf[Any], $out) }
          case None =>
            report.error(s"No inlined writer found for self-ref: ${s.typedName}")
            '{ () }

      case _ =>
        report.error(s"Unsupported RTypeRef: $ref")
        '{ () }

    // Phase 2: Patch the placeholder
    ref.refType match
      case '[w] =>
        realWriter = '{
          (in: Any, out: JsonOutput) =>
            val typedIn = in.asInstanceOf[w]
            ${ body }
        }
        ctx.generatedWriters.update(typedName, realWriter)

    body
