package co.blocke.scalajack
package json

import co.blocke.scala_reflection.{ReflectException, RType, RTypeRef, TypedName}
import co.blocke.scala_reflection.rtypes.TraitRType
import co.blocke.scala_reflection.reflect.rtypeRefs.*
import co.blocke.scala_reflection.reflect.*
import scala.quoted.*
import scala.quoted.staging.*

object ReflectUtil:

  /** This function takes the RType of a trait and an instance of T and does two things.
    * First it expresses the instance's class *in terms of* the trait's concrete type parameters (if any).
    * Then it generates a writer function for the now correctly-typed class.
    *
    * @param traitType
    * @param a
    * @param sb
    * @param cfg
    * @return
    */
  def inTermsOf[T](traitType: RType[?], a: T, sb: StringBuilder, cfg: JsonConfig) =
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
        case x                       => throw new ReflectException(s"${x.name} is not of type trait")

      inTermsOfRef.refType match
        case '[t] =>
          val asClassRef = inTermsOfRef.asInstanceOf[ScalaClassRef[t]].copy(renderTrait = Some(traitType.name))
          JsonWriter.writeJsonFn[t](asClassRef.asInstanceOf[RTypeRef[t]])
    }
    val writeFn = run(fn)
    writeFn.asInstanceOf[(T, StringBuilder, JsonConfig) => StringBuilder]
