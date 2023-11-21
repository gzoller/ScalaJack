package co.blocke.scalajack
package json
package writing

import co.blocke.scala_reflection.{RTypeRef, TypedName}
import co.blocke.scala_reflection.reflect.rtypeRefs.*
import scala.quoted.*
import scala.collection.mutable.{Map => MMap}
import internal.TreeNode

object JsonCodecMaker:

    def generateCodecFor[T](ref: RTypeRef[T])(using Quotes)(using tt: Type[T]) = 
        import quotes.reflect.* 

        // Cache generated method Symbols + an array of the generated functions (DefDef)
        case class MethodKey(ref: RTypeRef[?], isStringified: Boolean)  // <-- TODO: Not clear what isStringified does here...
        val methodSyms = new scala.collection.mutable.HashMap[MethodKey, Symbol]
        val methodDefs = new scala.collection.mutable.ArrayBuffer[DefDef]

        // Fantastic Dark Magic here--lifted from Jasoniter.  Props!  This thing will create a DefDef, and a Symbol to it.
        // The Symbol will let you call the generated function later from other macro-generated code.  The goal is to use
        // generated functions to create cleaner/faster macro code than what straight quotes/splices would create unaided.
        def makeFn[U: Type](methodKey: MethodKey, arg: Expr[U], out: Expr[JsonOutput])(f: (Expr[U], Expr[JsonOutput])=> Expr[Unit]): Expr[Unit] =
            // Get a symbol, if one already created for this key... else make one.
            Apply(Ref(methodSyms.getOrElse(methodKey, {
                val sym = Symbol.newMethod(Symbol.spliceOwner, "w" + methodSyms.size, // 'w' is for Writer!
                    MethodType(List("in", "out"))(_ => List(TypeRepr.of[U], TypeRepr.of[JsonOutput]), _ => TypeRepr.of[Unit]))
                methodSyms.update(methodKey, sym)
                methodDefs += DefDef(sym, params => {
                    val List(List(in, out)) = params
                    Some(f(in.asExprOf[U], out.asExprOf[JsonOutput]).asTerm.changeOwner(sym))
                })
                sym
            })), List(arg.asTerm, out.asTerm)).asExprOf[Unit]

        def genFnBody[T](r: RTypeRef[?], aE: Expr[T], out: Expr[JsonOutput])(using Quotes) = 
            r.refType match
                case '[b] => 
                    r match
                        case t: SeqRef[?] =>
                            makeFn[b](MethodKey(t, false), aE.asInstanceOf[Expr[b]], out) { (in, out) =>
                                t.elementRef.refType match
                                    case '[e] =>
                                        val tin = in.asExprOf[Seq[e]]
                                        '{
                                            $out.startArray()
                                            $tin.foreach{ i => 
                                                ${ genWriteVal('{ i }, t.elementRef.asInstanceOf[RTypeRef[e]], out) }
                                            }
                                            $out.endArray()
                                        }
                                }

                        case t: ScalaClassRef[?] =>
                            makeFn[b](MethodKey(t, false), aE.asInstanceOf[Expr[b]], out) { (in, out) =>
                                val tin = in.asExprOf[b]
                                val body = {
                                    val eachField = t.fields.map{ f =>
                                        f.fieldRef.refType match
                                            case '[z] =>
                                                val fname = Expr(f.name)
                                                val fieldValue = Select.unique(tin.asTerm, f.name).asExprOf[z]
                                                '{
                                                    $out.label($fname)
                                                    ${ genWriteVal(fieldValue, f.fieldRef.asInstanceOf[RTypeRef[z]], out)}
                                                }
                                        }
                                    if eachField.length == 1 then eachField.head 
                                    else Expr.block(eachField.init, eachField.last)
                                    }
                                '{
                                    $out.startObject()
                                    $body
                                    $out.endObject()
                                }
                            }

        def genWriteVal[T: Type](
            aE: Expr[T], 
            ref: RTypeRef[T],
            // types: List[TypeRepr], 
            // isStringified: Boolean,  // config option to wrap numbers, boolean, etc in "".  Not needed for now... we'll see later...
            // optWriteDiscriminator: Option[WriteDiscriminator],
            out: Expr[JsonOutput])(using Quotes): Expr[Unit] =
                val methodKey = MethodKey(ref, false)
                methodSyms.get(methodKey).map{ sym => // hit cache first... then match on Ref type
                    Apply(Ref(sym), List(aE.asTerm, out.asTerm)).asExprOf[Unit]
                    }.getOrElse(
                        ref match
                            case t: BooleanRef => '{ $out.value(${aE.asExprOf[Boolean]}) }
                            case t: IntRef     => '{ $out.value(${aE.asExprOf[Int]}) }
                            case t: StringRef  => '{ $out.value(${aE.asExprOf[String]}) }
                            case _             => 
                                println("Gen for: "+ref)
                                genFnBody(ref, aE, out)
                    )

        //================================================================
        // You've made it this far!  Ok, now we sew everything together. 
        // We generate a codec class and then kick off a deep traversal of
        // generation from the given root ref (refer waaay back at the top of this fn...).
        //================================================================
        val codecDef = '{ //FIXME: generate a type class instance using `ClassDef.apply` and `Symbol.newClass` calls after graduating from experimental API: https://www.scala-lang.org/blog/2022/06/21/scala-3.1.3-released.html
            new JsonCodec[T] {
                // def nullValue: A = ${genNullValue[A](rootTpe :: Nil)} // <- needed?

                // TBD... when we're ready to tackle reading!
                // def decodeValue(in: JsonReader, default: A): A = ${
                //     if (cfg.encodingOnly) '{ ??? }
                //     else genReadVal(rootTpe :: Nil, 'default, cfg.isStringified, false, 'in)
                // }

                def encodeValue(in: T, out: JsonOutput): Unit = ${
                   genWriteVal('in, ref, 'out)
                }
            }
        }.asTerm
        val neededDefs =
            // others here???  Refer to Jsoniter file JsonCodecMaker.scala
            methodDefs
        val codec = Block(neededDefs.toList, codecDef).asExprOf[JsonCodec[T]]
        println(s"Codec: ${codec.show}")
        codec