package co.blocke.scalajack
package json
package writing

import scala.quoted.*
import co.blocke.scala_reflection.reflect.rtypeRefs.*
import co.blocke.scala_reflection.RTypeRef
import scala.util.{Failure, Success, Try}
import shared.CodecBuildContext

object MaybeWrite:

  // Tests whether we should write something or not--mainly in the case of Option, or wrapped Option
  // Affected types: Option, java.util.Optional, Left/Right, Try/Failure
  // Returns Expr[Unit] containing either the original phrase (if ok to write) or the phrase
  // prepended with the type-appropriate runtime check.  This may seem like drama, but the idea
  // is to avoid slowing runtime down with extra "if" checks unless they're absolutely needed.
  //

  def maybeWrite[T: Type](ctx: CodecBuildContext, cfg: SJConfig, label: String, aE: Expr[T], ref: RTypeRef[T], out: Expr[JsonOutput]): Expr[Unit] =
    given Quotes = ctx.quotes

    val labelE = Expr(label)
    _maybeWrite[T](
      ctx,
      cfg,
      '{ $out.label($labelE) },
      aE,
      ref,
      out
    )

  def maybeWriteMap[K: Type, V: Type](
      ctx: CodecBuildContext,
      cfg: SJConfig,
      keyE: Expr[K],
      valueE: Expr[V],
      keyRef: RTypeRef[K],
      valueRef: RTypeRef[V],
      out: Expr[JsonOutput]
  ): Expr[Unit] =
    given Quotes = ctx.quotes

    keyRef.refType match
      case '[k] =>
        _maybeWrite[V](
          ctx,
          cfg,
          '{
            $out.maybeComma()
            ${ Writer.genWriteVal(ctx, cfg, keyE.asExprOf[k], keyRef.asInstanceOf[RTypeRef[k]], out, false, true) }
            $out.colon()
          },
          valueE,
          valueRef,
          out
        )

  private def handleOptional[O[_]: Type, T: Type](
      ctx: CodecBuildContext,
      cfg: SJConfig,
      prefix: Expr[Unit],
      optionExpr: Expr[O[T]],
      paramRef: RTypeRef[T],
      out: Expr[JsonOutput],
      isEmpty: Expr[O[T] => Boolean],
      get: Expr[O[T] => T]
  ): Expr[Unit] =
    given Quotes = ctx.quotes

    if !cfg.noneAsNull then
      '{
        if ! $isEmpty($optionExpr) then ${ _maybeWrite[T](ctx, cfg, prefix, '{ $get($optionExpr) }, paramRef, out) }
      }
    else
      '{
        if ! $isEmpty($optionExpr) then ${ _maybeWrite[T](ctx, cfg, prefix, '{ $get($optionExpr) }, paramRef, out) }
        else
          $prefix; $out.burpNull()
      }

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

  // Tests whether we should write something or not--mainly in the case of Option, or wrapped Option
  // Affected types: Option, java.util.Optional, Left/Right, Try/Failure
  // Returns Expr[Unit] containing either the original phrase (if ok to write) or the phrase
  // prepended with the type-appropriate runtime check.  This may seem like drama, but the idea
  // is to avoid slowing runtime down with extra "if" checks unless they're absolutely needed.
  private def _maybeWrite[T: Type](
      ctx: CodecBuildContext,
      cfg: SJConfig,
      prefix: Expr[Unit],
      aE: Expr[T],
      ref: RTypeRef[T],
      out: Expr[JsonOutput]
  ): Expr[Unit] =
    given Quotes = ctx.quotes

    ref match
      case t: ScalaOptionRef[?] =>
        t.optionParamType.refType match
          case '[e] =>
            val tin = aE.asExprOf[Option[e]]
            handleOptional[Option, e](ctx, cfg, prefix, tin, t.optionParamType.asInstanceOf[RTypeRef[e]], out, '{ (o: Option[e]) => o == null || o.isEmpty }, '{ (o: Option[e]) => o.get })

      case t: JavaOptionalRef[?] =>
        t.optionParamType.refType match
          case '[e] =>
            val tin = aE.asExprOf[java.util.Optional[e]]
            handleOptional[java.util.Optional, e](
              ctx,
              cfg,
              prefix,
              tin,
              t.optionParamType.asInstanceOf[RTypeRef[e]],
              out,
              '{ (o: java.util.Optional[e]) => o == null || !o.isPresent },
              '{ (o: java.util.Optional[e]) => o.get }
            )

      case t: AliasRef[?] if t.unwrappedType.isInstanceOf[OptionRef[?]] =>
        t.unwrappedType.refType match
          case '[e] =>
            val unwrappedExpr =
              '{ $aE.asInstanceOf[e] } // explicit alias erasure

            _maybeWrite(
              ctx,
              cfg,
              prefix,
              unwrappedExpr,
              t.unwrappedType.asInstanceOf[RTypeRef[e]],
              out
            )

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

      case t: AnyRef =>
        AnyWriter.isOkToWrite(ctx, cfg, prefix, aE, out)

      case _ =>
        ref.refType match
          case '[u] =>
            '{
              $prefix
              ${ Writer.genWriteVal[u](ctx, cfg, aE.asExprOf[u], ref.asInstanceOf[RTypeRef[u]], out) }
            }
