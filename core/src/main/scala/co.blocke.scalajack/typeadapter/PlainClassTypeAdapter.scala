package co.blocke.scalajack
package typeadapter

import model._
import ClassHelper._

import scala.collection.immutable.Map
import scala.collection.mutable
import scala.reflect.runtime.universe._
import scala.util.{Try, Failure}

case class PlainClassTypeAdapter[T](
  className: String,
  typeMembersByName: Map[String, ClassHelper.TypeMember[T]],
  fieldMembersByName: Map[String, ClassHelper.ClassFieldMember[T, Any]],
  nonConstructorFieldNames: List[String],
  argsInConstructor: Int,
  argsTemplate: Array[Any],
  fieldBitsTemplate: mutable.BitSet,
  constructorMirror: MethodMirror,
  isSJCapture: Boolean,
  dbCollectionName: Option[String],
  isScala: Boolean,
)(implicit taCache: TypeAdapterCache, tt: TypeTag[T])
    extends TypeAdapter[T]
    with ClassTypeAdapterBase[T]
    with Classish {

  val orderedFieldNames
    : List[String] = List.empty[String] // req for delimited but not used

  override def dbKeys: List[ClassFieldMember[T, Any]] =
    fieldMembersByName.values
      .filter(_.dbKeyIndex.isDefined)
      .toList
      .sortBy(_.dbKeyIndex.get)

  def read(parser: Parser): T =
    if (parser.peekForNull)
      null.asInstanceOf[T]
    else {
      // External type hint --> Substitute type field's type into the placeholder (i.e.'T') in the class' fields
      val substitutedTypeMembersIfAny =
        if (typeMembersByName.nonEmpty) {
          val fixedFields = substituteTypeMemberTypes(parser, taCache)
          this.copy(fieldMembersByName = fixedFields)
        } else
          this // No type members in this class... do nothing

      val (missingBits, args, captured) = parser.expectObject(
        substitutedTypeMembersIfAny,
        taCache.jackFlavor.defaultHint
      )

      // Call constructor with constructor args (may be 0)
      val constructorArgs =
        args.take(substitutedTypeMembersIfAny.argsInConstructor)

      // Are all constructor args present?
      val missingConstructorArgs = missingBits.filter(_ < argsInConstructor)
      if (missingConstructorArgs.nonEmpty) {
        parser.backspace()
        throw new ScalaJackError(
          parser.showError(
            s"Class $className missing required constructor fields: " + missingConstructorArgs
              .map(b => fieldMembersByName.values.find(_.index == b).get.name)
              .mkString(", ")
          )
        )
      }
      val asBuilt = constructorMirror.apply(constructorArgs: _*).asInstanceOf[T]

      if (isSJCapture)
        asBuilt.asInstanceOf[SJCapture].captured = captured

      nonConstructorFieldNames.foreach { fname =>
        fieldMembersByName(fname) match {
          case aField if aField.isOptional && missingBits(aField.index) =>
            missingBits -= aField.index // do nothing... missing optional field ok
          case aField
              if aField.isOptional => // try to set (may crash if set value == null and field is non-nullable)
            Try(
              aField
                .valueSet(asBuilt, args(aField.index))(tt, typeToClassTag[T])
            ) match { // could crash if this is a non-nullable field
              case Failure(_) =>
                parser.backspace()
                throw new ScalaJackError(
                  parser.showError(
                    s"Couldn't set field ${aField.name}'s value to null in this object."
                  )
                )
              case _ => // all fine
            }
          case aField if !missingBits(aField.index) => // try to set field
            aField.valueSet(asBuilt, args(aField.index))(tt, typeToClassTag[T])
          case _ => // missing required field fall-thru
        }
      }

      if (missingBits.nonEmpty) {
        parser.backspace()
        throw new ScalaJackError(
          parser.showError(
            s"Class $className missing required fields: " + missingBits
              .map(b => fieldMembersByName.values.find(_.index == b).get.name)
              .mkString(", ")
          )
        )
      }
      asBuilt
    }

  def write[WIRE](t: T,
                  writer: Writer[WIRE],
                  out: mutable.Builder[WIRE, WIRE]): Unit = {
    val extras =
      scala.collection.mutable.ListBuffer.empty[(String, ExtraFieldValue[_])]
    val typeMembersWithRealTypes = typeMembersByName
      .map {
        case (typeMemberName, tm) =>
          val tType = tm.typeSignature.toString
          fieldMembersByName.values.collectFirst {
            case f if f.declaredValueType.toString == tType =>
              val realValue = f.valueIn(t)
              val realType: Type = runtimeMirror(
                realValue.getClass.getClassLoader
              ).classSymbol(realValue.getClass).toType
              tm.copy(runtimeConcreteType = Some(realType))
          } match {
            case Some(tmWithActualType) =>
              val typeMemberValue = taCache.jackFlavor.typeValueModifier
                .unapply(tmWithActualType.runtimeConcreteType.get)
              extras.append(
                (
                  typeMemberName,
                  ExtraFieldValue(
                    typeMemberValue,
                    taCache.jackFlavor.stringTypeAdapter
                  )
                )
              )
              (tm.typeSignature, tmWithActualType.runtimeConcreteType.get)
            case None =>
              (tm.typeSignature, null)
          }
      }
      .filter(_._2 != null)

    val typeFixedFields =
      substituteTypeMemberTypes(typeMembersWithRealTypes, taCache)
    writer.writeObject(
      t,
      typeFixedFields.keys.toList,
      typeFixedFields,
      out,
      extras.toList
    )
  }
}
