package co.blocke.scalajack
package typeadapter
package classes

import model._
import util._

import scala.collection.mutable
import scala.collection.mutable.Builder

// This should come *after* SealedTraitTypeAdapter in the Context factory list, as all sealed traits are
// also traits, and this factory would pick them all up, hiding the sealed ones.
//
case class TraitTypeAdapterFactory(hintLabel: String, specificType: Option[Type] = None) extends TypeAdapterFactory.FromClassSymbol {

  override def typeAdapterOf[T](classSymbol: ClassSymbol, next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (specificType.map(_ == tt.tpe).getOrElse(true) && classSymbol.isTrait)
      TraitTypeAdapter(classSymbol.fullName, hintLabel, tt.tpe)
    else
      next.typeAdapterOf[T]
}

case class TraitTypeAdapter[T](
    traitName:       String,
    typeFieldName:   MemberName, // hint label
    polymorphicType: Type,
    hintModFn:       Option[BijectiveFunction[String, Type]] = None // optional string->type map (hint value modifier)
)(implicit tt: TypeTag[T], context: Context) extends TypeAdapter[T] {

  private val populatedConcreteTypeCache = new mutable.WeakHashMap[Type, Type]
  private val typeTypeAdapter = context.typeAdapterOf[Type]

  private def populateConcreteType(concreteType: Type): Type =
    populatedConcreteTypeCache.getOrElseUpdate(concreteType, Reflection.populateChildTypeArgs(tt.tpe, concreteType))

  // The battle plan here is:  Scan the keys of the object looking for type typeHintField.  Perform any (optional)
  // re-working of the hint value via hintModFn.  Look up the correct concete TypeAdapter based on the now-known type
  // and re-read the object as a case class.
  def read[WIRE](path: Path, reader: Transceiver[WIRE], isMapKey: Boolean): T = {
    reader.savePos()
    val concreteType = reader.lookAheadForField(typeFieldName)
      .map(typeHint => hintModFn.map(_.apply(typeHint)).getOrElse(typeTypeAdapter.read(path, reader, false)))
      .getOrElse(throw new ReadMissingError(path, s"No type hint found for trait $traitName", List(traitName)))
    reader.rollbackToSave()
    context.typeAdapter(concreteType).read(path, reader, isMapKey).asInstanceOf[T]
  }

  def write[WIRE](t: T, writer: Transceiver[WIRE], out: Builder[Any, WIRE]): Unit = {}

}
