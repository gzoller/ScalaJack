package co.blocke.scalajack

import co.blocke.scala_reflection.{RType, RTypeRef, TypedName, ReflectException}
import co.blocke.scala_reflection.rtypes.TraitRType
import co.blocke.scala_reflection.reflect.rtypeRefs.*
import co.blocke.scala_reflection.reflect.*
import scala.quoted.*
import scala.quoted.staging.*

object ReflectUtil:

  def makeTraitFn(): (String, Quotes) => Expr[List[?]] =
    (className: String, q: Quotes) =>
        import q.reflect.*
        implicit val q2 = q

        val clazz = Class.forName(className)
        val classRef = ReflectOnType(quotes)(quotes.reflect.TypeRepr.typeConstructorOf(clazz), false)(using scala.collection.mutable.Map.empty[TypedName, Boolean])
        println("HERE: "+classRef)

        Expr(Nil)



//   inline def mcr(f: (String => String) => String): String = ${ mcrImpl('f) }

//   def mcrImpl(ef: Expr[(String => String) => String])(using Quotes): Expr[String] =
//     import quotes.reflect.*
//     val customLambda = Lambda(
//         owner = Symbol.spliceOwner,
//         tpe = MethodType(List("x"))(
//         paramInfosExp = methodType => List(TypeRepr.of[String]),
//         resultTypeExp = methodType => TypeRepr.of[String]
//         ),
//         rhsFn = (sym: Symbol, paramRefs: List[Tree]) => {
//             val x = paramRefs.head.asExprOf[String]
//             '{ 
//                 ${
//                     Expr(t.toString)
//                 }
//                 "Hello, "+ $x 
//             }.asTerm
//         }
//     )
//     '{ $ef(${customLambda.asExprOf[String => String]}) }
    


// PLAN: Make a runtime-level fn that bundles of() and inTermsOf, keeps everything at the ref level and
// utilizes staging/Compiler for Quotes.  Not pretty, but cuts the burden of needing 2 calls to staging.






        /*
  def of(clazz: Class[?]): RType[?] =
    rtypeCache.getOrElse(
      clazz.getName, {
        val newRType = {
          val fn = (quotes: Quotes) ?=> reflect.ReflectOnType(quotes)(quotes.reflect.TypeRepr.typeConstructorOf(clazz), false)(using scala.collection.mutable.Map.empty[TypedName, Boolean]).expr
          run(fn)
        }.asInstanceOf[RType[?]]
        rtypeCache.synchronized {
          rtypeCache.put(clazz.getName, newRType)
        }
        newRType
      }
    )
        val classRType = of(clazz).asInstanceOf[rtypes.ScalaClassRType[_]]
    */

  def inTermsOf[T](traitType: RType[?], a: T, sb: StringBuilder, cfg: json.JsonConfig): Unit =
    given Compiler = Compiler.make(getClass.getClassLoader)

    val clazz = a.getClass

    val fn = (quotes: Quotes) ?=> {
      import quotes.reflect.*

      val classRef = ReflectOnType(quotes)(quotes.reflect.TypeRepr.typeConstructorOf(clazz), false)(using scala.collection.mutable.Map.empty[TypedName, Boolean]).asInstanceOf[ScalaClassRef[?]]

      val inTermsOfRef = traitType match
        case ttype: TraitRType[?] if ttype.typeParamSymbols.nonEmpty =>
          val seenBefore = scala.collection.mutable.Map.empty[TypedName, Boolean]
          val paths = classRef.typePaths.getOrElse(ttype.name, throw new ReflectException(s"No path in class ${classRef.name} for trait ${ttype.name}"))
          ttype.toType(quotes) match
            case '[t] =>
              val typeParamTypes = TypeSymbolMapper.runPath(quotes)(paths, TypeRepr.of[t])
              val classQuotedTypeRepr = TypeRepr.typeConstructorOf(clazz)
              ReflectOnType(quotes)(classQuotedTypeRepr.appliedTo(typeParamTypes))(using seenBefore)

        case traitRef: TraitRType[?] => classRef
        case x => throw new ReflectException(s"${x.name} is not of type trait")

      inTermsOfRef.refType match
        case '[t] =>
          val asClassRef = inTermsOfRef.asInstanceOf[ScalaClassRef[t]].copy(renderHint = true)
          json.JsonWriter.writeJsonFn[t](asClassRef.asInstanceOf[RTypeRef[t]])
    }
    val writeFn = run(fn)
    val writeFnTyped = writeFn.asInstanceOf[(T, StringBuilder, json.JsonConfig) => StringBuilder]
    writeFnTyped(a, sb, cfg)




    /*
    
    At compile-time:

        * Build a function that accepts a string and prints it
    
    At run-time:

        * Run the function with a.getClass.getName
    
    */