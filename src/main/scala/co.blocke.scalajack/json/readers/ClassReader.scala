package co.blocke.scalajack
package json
package readers

import co.blocke.scala_reflection.reflect.rtypeRefs.*
import co.blocke.scala_reflection.rtypes.*
import co.blocke.scala_reflection.{Clazzes, RTypeRef, TypedName}
import co.blocke.scala_reflection.Liftables.TypedNameToExpr
import scala.quoted.*
import scala.collection.mutable.HashMap
import scala.util.{Failure, Success, Try}

case class ClassReader(next: ReaderModule, root: ReaderModule) extends ReaderModule:

  def readerFn[T](ref: RTypeRef[T], isMapKey: Boolean = false)(using q: Quotes, tt: Type[T])(using cache: HashMap[Expr[TypedName], Expr[(JsonConfig, JsonParser) => Either[ParseError, ?]]]): Expr[(JsonConfig, JsonParser) => Either[ParseError, T]] =
    import quotes.reflect.*
    import Clazzes.*

    ref match
      case t: ScalaClassRef[T] =>
        if isMapKey then throw new JsonError("Class types cannot be map keys.")
        t.refType match
          case '[s] =>
            val parseTable = JsonReaderUtil.classParseMap[T](t, root)
            val instantiator = JsonReaderUtil.classInstantiator[T](t)
            val classFn = '{ (j: JsonConfig, p: JsonParser) =>
              val rtype = ${ t.expr }.asInstanceOf[ScalaClassRType[T]]
              val presetFieldValues = scala.collection.mutable.HashMap.empty[String, Any]
              rtype.fields.foreach(f =>
                if f.fieldType.clazz == OptionClazz then presetFieldValues.put(f.name, None)
                else if f.asInstanceOf[ScalaFieldInfo].defaultValueAccessorName.isDefined then
                  val (companion, accessor) = f.asInstanceOf[ScalaFieldInfo].defaultValueAccessorName.get

                  // Have to use Java reflection here to get default value--Scala compiler won't have access to companion
                  // or accessor if we do a ${} block, and using compiler staging would murder performance.
                  val defaultValue = {
                    val c = Class.forName(companion)
                    val cons = c.getDeclaredConstructor()
                    cons.setAccessible(true)
                    val m = c.getMethod(accessor)
                    m.setAccessible(true)
                    m.invoke(cons.newInstance())
                  }
                  presetFieldValues.put(f.name, defaultValue)
              )
              val classFieldMap = $parseTable(p) // Map[String, JsonConfig => Either[ParseError, ?]]
              p.expectClass(j, classFieldMap, presetFieldValues)
                .flatMap(fieldValues =>
                  scala.util.Try($instantiator(fieldValues.toMap)) match // instantiate the class here!!!
                    case Success(v) => Right(v)
                    case Failure(e) => Left(JsonParseError(p.showError(s"Unable to instantiate class at position [${p.getPos - 1}] with message ${e.getMessage}")))
                )
            }
            cache.put(Expr(t.typedName), classFn)
            classFn

      case t =>
        next.readerFn[T](t)
