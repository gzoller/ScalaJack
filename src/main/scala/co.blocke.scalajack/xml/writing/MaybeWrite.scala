package co.blocke.scalajack
package xml
package writing

import shared.CodecBuildContext
import scala.quoted.*
import co.blocke.scala_reflection.reflect.rtypeRefs.*
import co.blocke.scala_reflection.RTypeRef
import scala.util.{Failure, Success, Try}

object MaybeWrite:

  // Tests whether we should write something or not--mainly in the case of Option, or wrapped Option
  // Affected types: Option, java.util.Optional, Left/Right, Try/Failure
  // Returns Expr[Unit] containing either the original phrase (if ok to write) or the phrase
  // prepended with the type-appropriate runtime check.  This may seem like drama, but the idea
  // is to avoid slowing runtime down with extra "if" checks unless they're absolutely needed.
  //

//  def maybeWrite[T: Type](ctx: CodecBuildContext, cfg: SJConfig, labelE: Expr[String], aE: Expr[T], ref: RTypeRef[T], out: Expr[XmlOutput])(using Quotes): Expr[Unit] =
//    given Quotes = ctx.quotes
//
//    _maybeWrite[T](
//      ctx,
//      cfg,
//      labelE,
//      aE,
//      ref,
//      out
//    )

  def maybeWriteField[V: Type](
      ctx: CodecBuildContext,
      cfg: SJConfig,
      fieldName: String,
      valueE: Expr[V],
      valueRef: RTypeRef[V],
      out: Expr[XmlOutput],
      entryLabel: Option[String] = None
  ): Expr[Unit] =
    given Quotes = ctx.quotes

    // Collections are special. Normally we print <field>...</field> and let the call to _maybeWrite supply the innards.
    // For collections tho, it isn't guaranteed we emit the wrapper <field>. So don't emit anything and pass responsibility
    // for wrapping, or not, to _maybeWrite.
    val (prefix, postfix) = valueRef match {
      case _: CollectionRef[?] if entryLabel.isEmpty =>
        (
          '{ () },
          '{ () }
        )
      case _ =>
        val fieldNameE = Expr(fieldName)
        (
          '{
            $out.startElement($fieldNameE)
          },
          '{
            $out.endElement($fieldNameE)
          }
        )
    }
    _maybeWrite[V](
      ctx,
      cfg,
      valueE,
      valueRef,
      out,
      fieldName,
      prefix,
      postfix,
      entryLabel
    )

  def maybeWriteMapEntry[K: Type, V: Type](
      ctx: CodecBuildContext,
      cfg: SJConfig,
      mapElementLabel: String,
      entryLabel: String,
      keyE: Expr[K],
      valueE: Expr[V],
      keyRef: RTypeRef[K],
      valueRef: RTypeRef[V],
      out: Expr[XmlOutput]
  ): Expr[Unit] =
    given Quotes = ctx.quotes

    keyRef.refType match
      case '[k] =>
        val entryLabelE = Expr(entryLabel)
        val pprefix = '{
          $out.openElement($entryLabelE)
          $out.attribute("key")
        }
        val ppostfix = '{
          $out.closeAttribute()
          $out.closeElement()
        }
        _maybeWrite[V](
          ctx,
          cfg,
          valueE,
          valueRef,
          out,
          mapElementLabel,
          Writer.genWriteVal(ctx, cfg, keyE.asExprOf[k], keyRef.asInstanceOf[RTypeRef[k]], out, false, true, pprefix, ppostfix, mapElementLabel),
          '{
            $out.endElement($entryLabelE)
          },
          Some(entryLabel)
        )

  private def handleOptional[O[_]: Type, T: Type](
      ctx: CodecBuildContext,
      cfg: SJConfig,
      optionExpr: Expr[O[T]],
      paramRef: RTypeRef[T],
      out: Expr[XmlOutput],
      isEmpty: Expr[O[T] => Boolean],
      get: Expr[O[T] => T],
      prefix: Expr[Unit],
      postfix: Expr[Unit],
      fieldLabel: String,
      entryLabel: Option[String]
  ): Expr[Unit] =
    given Quotes = ctx.quotes

    if !cfg.noneAsNull then
      '{
        if ! $isEmpty($optionExpr) then ${ _maybeWrite[T](ctx, cfg, '{ $get($optionExpr) }, paramRef, out, fieldLabel, prefix, postfix, entryLabel) }
      }
    else
      '{
        if ! $isEmpty($optionExpr) then ${ _maybeWrite[T](ctx, cfg, '{ $get($optionExpr) }, paramRef, out, fieldLabel, prefix, postfix, entryLabel) }
        else
          $prefix; $out.burpNull()
      }

  /*

      Maybe write:

      <map>  <- has wrapper
        <entry "key">value</entry>
      </map>

      <list> <- has wrapper
        <item>foo</item>
      <list>

      <class...>

      <field>value</field>

  private def handleTry[T: Type](
                                  ctx: CodecBuildContext,
                                  cfg: SJConfig,
                                  prefix: Expr[Unit],
                                  tryExpr: Expr[Try[T]],
                                  paramRef: RTypeRef[T],
                                  out: Expr[JsonOutput]
                                ): Expr[Unit] =
    given Quotes = ctx.quotes

    '{
      $tryExpr match
        case Failure(v) =>
          ${
            cfg.tryFailureHandling match
              case TryPolicy.AS_NULL         => '{ $prefix; $out.burpNull() }
              case TryPolicy.ERR_MSG_STRING  => '{ $prefix; $out.value("Try Failure with msg: " + v.getMessage) }
              case TryPolicy.THROW_EXCEPTION => '{ throw v }
          }
        case Success(v) => ${ _maybeWrite[T](ctx, cfg, prefix, '{ v }, paramRef, out) }
    }


  private def handleLR[T: Type](
                                 ctx: CodecBuildContext,
                                 cfg: SJConfig,
                                 t: LeftRightRef[?],
                                 aE: Expr[T],
                                 out: Expr[JsonOutput],
                                 prefix: Expr[Unit]
                               ): Expr[Unit] =
    given Quotes = ctx.quotes

    t match
      case t if t.lrkind == LRKind.EITHER =>
        t.refType match
          case '[u] =>
            val tin = aE.asExprOf[u]
            t.rightRef.refType match
              case '[rt] =>
                cfg.eitherLeftHandling match
                  case EitherLeftPolicy.AS_NULL =>
                    '{
                      if $tin == null then $out.burpNull()
                      $tin match
                        case Left(_) =>
                          $prefix
                          $out.burpNull()
                        case Right(v) =>
                          ${ _maybeWrite[rt](ctx, cfg, prefix, '{ v.asInstanceOf[rt] }, t.rightRef.asInstanceOf[RTypeRef[rt]], out) }
                    }

                  case EitherLeftPolicy.ERR_MSG_STRING =>
                    '{
                      if $tin == null then $out.burpNull()
                      $tin match
                        case Left(err) =>
                          $prefix
                          $out.value("Left Error: " + err.toString)
                        case Right(v) =>
                          ${ _maybeWrite[rt](ctx, cfg, prefix, '{ v.asInstanceOf[rt] }, t.rightRef.asInstanceOf[RTypeRef[rt]], out) }
                    }

                  case EitherLeftPolicy.THROW_EXCEPTION =>
                    '{
                      if $tin == null then $out.burpNull()
                      $tin match
                        case Left(err) => throw new EitherLeftError("Left Error: " + err.toString)
                        case Right(v) =>
                          ${ _maybeWrite[rt](ctx, cfg, prefix, '{ v.asInstanceOf[rt] }, t.rightRef.asInstanceOf[RTypeRef[rt]], out) }
                    }

                  case EitherLeftPolicy.AS_VALUE =>
                    t.leftRef.refType match
                      case '[lt] =>
                        '{
                          if $tin == null then $out.burpNull()
                          $tin match
                            case Left(v) =>
                              ${ _maybeWrite[lt](ctx, cfg, prefix, '{ v.asInstanceOf[lt] }, t.leftRef.asInstanceOf[RTypeRef[lt]], out) }
                            case Right(v) =>
                              ${ _maybeWrite[rt](ctx, cfg, prefix, '{ v.asInstanceOf[rt] }, t.rightRef.asInstanceOf[RTypeRef[rt]], out) }
                        }

      case _ => // Union or Intersection
        t.refType match
          case '[e] =>
            t.rightRef.refType match
              case '[rt] =>
                t.leftRef.refType match
                  case '[lt] =>
                    val tin = aE.asExprOf[e]
                    '{
                      if $tin == null then $out.burpNull()
                      else
                        $out.mark()
                        scala.util.Try {
                          ${ _maybeWrite[rt](ctx, cfg, prefix, '{ $tin.asInstanceOf[rt] }, t.rightRef.asInstanceOf[RTypeRef[rt]], out) }
                        } match
                          case scala.util.Success(_) => ()
                          case scala.util.Failure(_) =>
                            $out.revert()
                            ${ _maybeWrite[lt](ctx, cfg, prefix, '{ $tin.asInstanceOf[lt] }, t.leftRef.asInstanceOf[RTypeRef[lt]], out) }
                    }
   */

  // Tests whether we should write something or not--mainly in the case of Option, or wrapped Option
  // Affected types: Option, java.util.Optional, Left/Right, Try/Failure
  // Returns Expr[Unit] containing either the original phrase (if ok to write) or the phrase
  // prepended with the type-appropriate runtime check.  This may seem like drama, but the idea
  // is to avoid slowing runtime down with extra "if" checks unless they're absolutely needed.
  private def _maybeWrite[T: Type](
      ctx: CodecBuildContext,
      cfg: SJConfig,
      aE: Expr[T],
      ref: RTypeRef[T],
      out: Expr[XmlOutput],
      fieldLabel: String,
      prefix: Expr[Unit],
      postfix: Expr[Unit],
      entryLabel: Option[String] = None
  ): Expr[Unit] =
    given Quotes = ctx.quotes

    ref match
      case t: ScalaOptionRef[?] =>
        t.optionParamType.refType match
          case '[e] =>
            val tin = aE.asExprOf[Option[e]]
            handleOptional[Option, e](
              ctx,
              cfg,
              tin,
              t.optionParamType.asInstanceOf[RTypeRef[e]],
              out,
              '{ (o: Option[e]) => o.isEmpty },
              '{ (o: Option[e]) => o.get },
              prefix,
              postfix,
              fieldLabel,
              entryLabel
            )

      case t: JavaOptionalRef[?] =>
        t.optionParamType.refType match
          case '[e] =>
            val tin = aE.asExprOf[java.util.Optional[e]]
            handleOptional[java.util.Optional, e](
              ctx,
              cfg,
              tin,
              t.optionParamType.asInstanceOf[RTypeRef[e]],
              out,
              '{ (o: java.util.Optional[e]) => !o.isPresent },
              '{ (o: java.util.Optional[e]) => o.get },
              prefix,
              postfix,
              fieldLabel,
              entryLabel
            )

      /*
      case t: TryRef[?] =>
        t.tryRef.refType match
          case '[e] =>
            val tin = aE.asExprOf[scala.util.Try[e]]
            handleTry[e](ctx, cfg, prefix, tin, t.tryRef.asInstanceOf[RTypeRef[e]], out)

      case t: LeftRightRef[?] =>
        t.refType match
          case '[u] =>
            t.leftRef.refType match
              case '[lt] =>
                t.rightRef.refType match
                  case '[rt] =>
                    handleLR(ctx, cfg, t, aE, out, prefix)
       */

//      case t: AnyRef =>
//        AnyWriter.isOkToWrite(ctx, cfg, prefix, aE, out)

      case _ =>
        ref.refType match
          case '[u] =>
            Writer.genWriteVal[u](ctx, cfg, aE.asExprOf[u], ref.asInstanceOf[RTypeRef[u]], out, false, false, prefix, postfix, fieldLabel, entryLabel)
