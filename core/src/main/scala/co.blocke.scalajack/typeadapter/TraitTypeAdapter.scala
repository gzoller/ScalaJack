package co.blocke.scalajack
package typeadapter

import model._
import classes._
import co.blocke.scala_reflection._
import co.blocke.scala_reflection.info.TraitInfo

import scala.collection.mutable

// This should come *after* SealedTraitTypeAdapter in the Context factory list, as all sealed traits are
// also traits, and this factory would pick them all up, hiding the sealed ones.
//
object TraitTypeAdapterFactory extends TypeAdapterFactory:

  def matches(concrete: RType): Boolean = 
    concrete match {
      case _: TraitInfo => true
      case _ => false
    }

  def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[_] =
    TraitTypeAdapter(concrete, taCache.jackFlavor.getHintLabelFor(concrete))


case class TraitTypeAdapter[T](
    info:            RType,
    hintLabel:       String
)(implicit taCache: TypeAdapterCache) extends TypeAdapter[T] with Classish:

  inline def calcTA(c: Class[_]): ClassTypeAdapterBase[T] =
    taCache.typeAdapterOf(RType.inTermsOf(c, info.asInstanceOf[TraitInfo])).asInstanceOf[ClassTypeAdapterBase[T]]
  
  // The battle plan here is:  Scan the keys of the object looking for type typeHintField.  Perform any (optional)
  // re-working of the hint value via hintModFn.  Look up the correct concete TypeAdapter based on the now-known type
  // and re-read the object as a case class.
  def read(parser: Parser): T =
    if (parser.peekForNull)
      null.asInstanceOf[T]
    else {
      val concreteClass = parser.scanForHint(
        hintLabel,
        taCache.jackFlavor.hintValueModifiers.getOrElse(info.name, DefaultHintModifier)
      ).asInstanceOf[Class[T]]
      val ccta = calcTA(concreteClass)
      ccta.read(parser).asInstanceOf[T]
    }

  def write[WIRE](
      t:      T,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit =
    if (t == null)
      writer.writeNull(out)
    else {
      val ccta = calcTA(t.getClass)
      val hintValue = taCache.jackFlavor.hintValueModifiers
        .getOrElse(info.name, DefaultHintModifier)
        .unapply(t.getClass.getName)
      writer.writeObject(
        t, 
        ccta.orderedFieldNames, 
        ccta.fieldMembersByName, 
        out, 
        List(
          (
            hintLabel,
            ExtraFieldValue(
              hintValue,
              taCache.jackFlavor.stringTypeAdapter
            )
          )
        )
      )
    }