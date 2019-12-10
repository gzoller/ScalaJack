package co.blocke.scalajack
package typeadapter

import model._
import util.Reflection

import scala.collection.mutable
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe._

// This should come *after* SealedTraitTypeAdapter in the Context factory list, as all sealed traits are
// also traits, and this factory would pick them all up, hiding the sealed ones.
//
object TraitTypeAdapterFactory extends TypeAdapterFactory.FromClassSymbol {

  override def typeAdapterOf[T](
      classSymbol: ClassSymbol,
      next:        TypeAdapterFactory
  )(implicit taCache: TypeAdapterCache, tt: TypeTag[T]): TypeAdapter[T] =
    if (classSymbol.isTrait) {
      TraitTypeAdapter(
        classSymbol.fullName,
        taCache.jackFlavor.getHintLabelFor(tt.tpe),
        tt.tpe,
        ActiveTypeParamWeaver(tt.tpe)
      //        if (tt.tpe.typeArgs.nonEmpty) ActiveTypeParamWeaver(tt.tpe) else NoOpTypeParamWeaver()
      )
    } else
      next.typeAdapterOf[T]
}

trait TypeParamWeaver {
  def populateConcreteType(concreteType: Type): Type
}
case class NoOpTypeParamWeaver() extends TypeParamWeaver {
  def populateConcreteType(concreteType: Type): Type = concreteType
}
case class ActiveTypeParamWeaver(polymorphicType: Type)
  extends TypeParamWeaver {
  private val populatedConcreteTypeCache = new mutable.WeakHashMap[Type, Type]

  def populateConcreteType(concreteType: Type): Type =
    populatedConcreteTypeCache.getOrElseUpdate(
      concreteType,
      Reflection.populateChildTypeArgs(polymorphicType, concreteType)
    )
}

case class TraitTypeAdapter[T](
    traitName:       String,
    hintLabel:       String,
    polymorphicType: Type,
    typeParamWeaver: TypeParamWeaver
)(implicit taCache: TypeAdapterCache)
  extends TypeAdapter[T]
  with Classish {

  // The battle plan here is:  Scan the keys of the object looking for type typeHintField.  Perform any (optional)
  // re-working of the hint value via hintModFn.  Look up the correct concete TypeAdapter based on the now-known type
  // and re-read the object as a case class.
  def read(parser: Parser): T =
    if (parser.peekForNull)
      null.asInstanceOf[T]
    else {
      val concreteType = parser.scanForHint(
        hintLabel,
        taCache.jackFlavor.hintValueModifiers
          .getOrElse(polymorphicType, DefaultHintModifier)
      )
      taCache
        .typeAdapter(typeParamWeaver.populateConcreteType(concreteType))
        .read(parser)
        .asInstanceOf[T]
    }

  def write[WIRE](
      t:      T,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit = {
    if (t == null)
      writer.writeNull(out)
    else {
      val concreteType = currentMirror.classSymbol(t.getClass).toType
      val populatedConcreteType =
        typeParamWeaver.populateConcreteType(concreteType)
      taCache
        .typeAdapter(populatedConcreteType)
        .asInstanceOf[TypeAdapter[T]] match {
          case cc: CaseClassTypeAdapter[T] =>
            val hintValue = taCache.jackFlavor.hintValueModifiers
            .getOrElse(polymorphicType, DefaultHintModifier)
            .unapply(concreteType)
            writer.writeObject(
              t,
              cc.orderedFieldNames,
              cc.fieldMembersByName,
              out,
              List(
                (
                  hintLabel,
                  ClassHelper.ExtraFieldValue(
                    hintValue,
                    taCache.jackFlavor.stringTypeAdapter
                  )
                )
              )
            )
        }
    }
  }
}
