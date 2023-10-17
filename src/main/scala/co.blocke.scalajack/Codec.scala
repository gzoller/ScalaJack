package co.blocke.scalajack

import co.blocke.scala_reflection.TypedName
import co.blocke.scala_reflection.reflect.ReflectOnType
import scala.quoted.*
import quoted.Quotes
import json.*

object Codec:

    inline def write[T](t: T): String = ${ writeImpl[T]('t) }

    def writeImpl[T:Type](t: Expr[T])(using q: Quotes): Expr[String] =
        import quotes.reflect.*

        val rtRef = ReflectOnType[T](q)(TypeRepr.of[T])(using scala.collection.mutable.Map.empty[TypedName, Boolean])
        val fn = JsonWriter.writeJsonFn[T](rtRef)
        '{ 
            val sb = new StringBuilder()
            $fn($t, sb).toString 
        }
