package co.blocke.scalajack
package csv

import scala.reflect.runtime.universe.{ Type, TypeTag }
import java.lang.{ UnsupportedOperationException => UOE }

import typeadapter.CaseClassTypeAdapter
import org.json4s.JsonAST.JValue

case class CSVFlavor() extends {
  val customAdapters: List[TypeAdapterFactory] = List.empty[TypeAdapterFactory]
  val hintMap: Map[Type, String] = Map.empty[Type, String]
  val hintModifiers: Map[Type, HintModifier] = Map.empty[Type, HintModifier]
  val typeModifier: Option[HintModifier] = None
  val parseOrElseMap: Map[Type, Type] = Map.empty[Type, Type]
  val defaultHint: String = "_hint"
  val isCanonical: Boolean = true
  val secondLookParsing: Boolean = false
} with ScalaJackLike[JValue, String] {

  def withAdapters(ta: TypeAdapterFactory*) = throw new UOE("Not available for CSV formatting")
  def withHints(h: (Type, String)*) = throw new UOE("Not available for CSV formatting")
  def withHintModifiers(hm: (Type, HintModifier)*) = throw new UOE("Not available for CSV formatting")
  def withDefaultHint(hint: String) = throw new UOE("Not available for CSV formatting")
  def withTypeModifier(tm: HintModifier) = throw new UOE("Not available for CSV formatting")
  def withSecondLookParsing() = this
  def parseOrElse(poe: (Type, Type)*) = throw new UOE("Not available for CSV formatting")
  def isCanonical(canonical: Boolean) = throw new UOE("Not available for CSV formatting")

  implicit val ops = CSVOps

  // We use withMapValue here to force handling of Optional class fields for CSV.  For Json, optional fields are just dropped/ignored,
  // but for csv we need to emit an empty field.  This flag forces CSV-correct handling within OptionSerializer
  implicit val guidance: SerializationGuidance = SerializationGuidance().withMapValue()

  // Reading CSV is wonky.  We need fieldname/value pairs and CVS only gives us ordered values, sooo....
  // We need to grab the internals of the case class, specifically the fields, then (in order) create the
  // fieldname->value map required by the case class deserializer.  Additionally there may be some type slip 'n slide
  // as CSV is free-form, so we need to do some smart-matching on the AST types against the required field types.
  def readSafely[T](csv: String)(implicit tt: TypeTag[T]): Either[DeserializationFailure, T] = {
    val ccta = context.typeAdapterOf[T].as[CaseClassTypeAdapter[T]]
    val fields = ccta.members.map(member => (member.name, member.asInstanceOf[CaseClassTypeAdapter.FieldMember[_, _]].valueType))
    val listBuf = scala.collection.mutable.ListBuffer.empty[(String, JValue)]

    this.ops.parse(csv) match {
      case AstArray(e) =>
        ops.foreachArrayElement(e, { (pos, value) =>
          listBuf.append((fields(pos)._1, fixedValue(value, fields(pos)._2)))
        })
        val objAst = ops.applyObject { appendField =>
          for ((fieldName, fieldValue) <- listBuf) {
            appendField(fieldName, fieldValue)
          }
        }
        ccta.deserializer.deserialize(Path.Root, objAst) match {
          case DeserializationSuccess(t)       => Right(t.get)
          case failure: DeserializationFailure => Left(failure)
        }

      case AstNull() => Right(null.asInstanceOf[T])
      case _         => Left(DeserializationFailure(Path.Root, DeserializationError.Unexpected("Unable to successfully parse this CSV", ccta.deserializer)))
    }
  }

  // Fuzzy "safe" conversions to handle the fact that CSV is pretty free-from
  private def fixedValue(astValue: JValue, ccFieldType: Type): JValue =
    astValue match {
      case AstDouble(d) if typeOf[String] == ccFieldType => AstString(d.toString)
      case AstLong(n) if typeOf[String] == ccFieldType => AstString(n.toString)
      case AstBoolean(b) if typeOf[String] == ccFieldType => AstString(b.toString)
      case _ => astValue
    }

  def render[T](value: T)(implicit valueTypeTag: TypeTag[T]): String = {
    val typeAdapter = context.typeAdapterOf[T]
    val serializer = typeAdapter.serializer
    serializer.serialize[JValue, String](TypeTagged(value, valueTypeTag.tpe)) match {
      case SerializationSuccess(objectOutput) =>
        objectOutput match {
          case AstObject(obj) =>
            val valuesOnly = ops.map(obj.asInstanceOf[ops.ObjectFields], { (_, element) => element })
            ops.renderCompact(ops.applyArray(valuesOnly), this)
          case AstArray(_) => ops.renderCompact(objectOutput, this)
          case AstNull()   => ""
          case _           => ""
        }
      case SerializationFailure(f) if f == Seq(SerializationError.Nothing) => ""
    }
  }

  def parse(csv: String): JValue = ops.parse(csv)

  def emit(ast: JValue): String = ops.renderCompact(ast, this)

}
