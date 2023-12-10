package co.blocke.scalajack
package json
package writing

import co.blocke.scala_reflection.{RType, TypedName}
import co.blocke.scala_reflection.rtypes.*
import scala.jdk.CollectionConverters.*
import org.apache.commons.text.StringEscapeUtils
import org.apache.commons.lang3.text.translate.CharSequenceTranslator
import scala.util.*

/**
  * Writing an Any-typed value is a hot mess.  You don't know until runtime exactly what the target object will actually
  * be, so all the compile-time macro magic will be useless.  The sad part is that in many ways, most of the macro logic
  * must be re-written here, purely as a runtime ability.
  */

object AnyWriter:

    def writeAny( target: Any, out: JsonOutput, inTuple: Boolean = false ): Unit = 
        // val rt = RType.of(target.getClass)
        target match
            case null => out.burpNull()
            case v: BigDecimal => out.value(v)
            case v: BigInt => out.value(v)
            case v: Boolean => out.value(v)
            case v: Byte => out.value(v)
            case v: Char => out.value(v)
            case v: Double => out.value(v)
            case v: Float => out.value(v)
            case v: Int => out.value(v)
            case v: Long => out.value(v)
            case v: Short => out.value(v)
            case v: String => out.value(StringEscapeUtils.escapeJson(v))
            case v: java.lang.Boolean => out.value(v)
            case v: java.lang.Byte => out.value(v)
            case v: java.lang.Character => out.value(v)
            case v: java.lang.Double => out.value(v)
            case v: java.lang.Float => out.value(v)
            case v: java.lang.Integer => out.value(v)
            case v: java.lang.Long => out.value(v)
            case v: java.lang.Short => out.value(v)
            case v: java.lang.Number => out.value(v)
            case v: java.time.Duration => out.value(v)
            case v: java.time.Instant => out.value(v)
            case v: java.time.LocalDate => out.value(v)
            case v: java.time.LocalDateTime => out.value(v)
            case v: java.time.LocalTime => out.value(v)
            case v: java.time.MonthDay => out.value(v)
            case v: java.time.OffsetDateTime => out.value(v)
            case v: java.time.OffsetTime => out.value(v)
            case v: java.time.Period => out.value(v)
            case v: java.time.Year => out.value(v)
            case v: java.time.YearMonth => out.value(v)
            case v: java.time.ZoneOffset => out.value(v)
            case v: java.time.ZonedDateTime => out.value(v)
            case v: java.time.ZoneId => out.value(v)
            case v: java.util.UUID => out.value(v)

            case v: Array[_] =>
                out.startArray()
                v.map( e => writeAny(e,out) )
                out.endArray()

            case v: Set[_] =>
                out.startArray()
                v.map( e => writeAny(e,out) )
                out.endArray()

            case v: Seq[_] =>
                out.startArray()
                v.map( e => writeAny(e,out) )
                out.endArray()

            case v: Map[_,_] => 
                out.startObject()
                v.map{ case(k,v) => okToWrite(k.toString, v, out) }
                out.endObject()

            case v: Option[_] =>
                v match
                    case None => 
                        if inTuple then out.burpNull()
                        else ()
                    case Some(v2) => writeAny(v2,out)
                
            case v: Either[_,_] =>
                v match
                    case Left(_) => 
                        if inTuple then out.burpNull()
                        else ()
                    case Right(v2) => writeAny(v2,out)

            case v: Try[_] =>
                v match
                    case Failure(_) => 
                        if inTuple then out.burpNull()
                        else ()
                    case Success(v2) => writeAny(v2,out)

            case v: Tuple =>
                val varr = v.toArray
                out.startArray()
                varr.foreach( v2 => writeAny(v2,out,true) )
                out.endArray()
                
            case v =>
                val rt = RType.of(v.getClass)
                rt match
                    case t: ScalaClassRType[_] =>
                        out.startObject()
                        t.fields.map(f =>
                            val field = f.asInstanceOf[ScalaFieldInfo]
                            val m = v.getClass.getMethod(field.name)
                            m.setAccessible(true)
                            val fieldValue = m.invoke(v)
                            val fieldName = f.annotations.get("co.blocke.scalajack.Change").flatMap(_.get("name")).getOrElse(f.name)
                            okToWrite(fieldName, fieldValue, out)
                            )
                        out.endObject()
                    case _ => throw new JsonUnsupportedType("Class "+v.getClass.getName+" not supported for Any type")

    def okToWrite(label:String, value: Any, out: JsonOutput): Unit =
        value match
            case None => ()
            case Left(_) => ()
            case Some(v2) => okToWrite(label, v2, out)
            case _ =>
                out.label(label)
                writeAny(value,out)
