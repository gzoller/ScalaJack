package co.blocke.scalajack
package typeadapter

import model._

import scala.reflect.runtime.universe._
import scala.collection.mutable
import model.ClassHelper.ExtraFieldValue

case class CaseClassTypeAdapter[T](
    className:          String,
    typeMembersByName:  Map[String, ClassHelper.TypeMember[T]],
    orderedFieldNames:  List[String],
    fieldMembersByName: Map[String, ClassHelper.ClassFieldMember[T, Any]],
    argsTemplate:       Array[Any],
    fieldBitsTemplate:  mutable.BitSet,
    constructorMirror:  MethodMirror,
    isSJCapture:        Boolean,
    dbCollectionName:   Option[String]
)(implicit taCache: TypeAdapterCache, tt: TypeTag[T])
  extends TypeAdapter[T]
  with ClassTypeAdapterBase[T]
  with Classish {

  override val isCaseClass = true;

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

      val (foundBits, args, captured) = parser.expectObject(
        substitutedTypeMembersIfAny,
        taCache.jackFlavor.defaultHint
      )
      if (foundBits.isEmpty) {
        val asBuilt = constructorMirror.apply(args: _*).asInstanceOf[T]
        if (isSJCapture)
          asBuilt.asInstanceOf[SJCapture].captured = captured
        asBuilt
      } else {
        parser.backspace()
        throw new ScalaJackError(
          parser.showError(
            s"Class $className missing required fields: " + foundBits
              .map(b => orderedFieldNames(b))
              .mkString(", ")
          )
        )
      }
    }

  def write[WIRE](
      t:      T,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit = {
    val extras = typeMembersByName
      .map {
        case (typeMemberName, tm) =>
          (
            typeMemberName,
            ExtraFieldValue(
              taCache.jackFlavor.typeValueModifier.unapply(tm.baseType),
              taCache.jackFlavor.stringTypeAdapter
            )
          )
      }
    writer.writeObject(
      t,
      orderedFieldNames,
      fieldMembersByName,
      out,
      extras.toList
    )
  }

  // Used by AnyTypeAdapter to insert type hint (not normally needed) into output so object
  // may be reconstituted on read
  def writeWithHint[WIRE](
      t:      T,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit = {
    val hintValue = t.getClass.getName
    val hintLabel = taCache.jackFlavor.getHintLabelFor(tt.tpe)
    val extra = List(
      (
        hintLabel,
        ClassHelper
        .ExtraFieldValue(hintValue, taCache.jackFlavor.stringTypeAdapter)
      )
    )
    writer.writeObject(t, orderedFieldNames, fieldMembersByName, out, extra)
  }
}
