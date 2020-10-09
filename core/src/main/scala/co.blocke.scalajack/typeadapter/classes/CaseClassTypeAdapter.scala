package co.blocke.scalajack
package typeadapter
package classes

import model._
import co.blocke.scala_reflection.info._
import co.blocke.scala_reflection._

import scala.collection.mutable


case class CaseClassTypeAdapter[T](
    info:               RType,
    fieldMembersByName: Map[String, ClassFieldMember[_,_]],
    argsTemplate:       Array[Object],
    fieldBitsTemplate:  mutable.BitSet,
    typeMembersByName:  Map[String, TypeMemberInfo],
    orderedFieldNames:  List[String]
    // dbCollectionName:   Option[String]
)(implicit taCache: TypeAdapterCache) extends ScalaClassTypeAdapter[T]:

  override val isCaseClass = true;

  private val classInfo = info.asInstanceOf[ScalaCaseClassInfo]

  inline def constructWith(args: List[Object]): T =
    val const = classInfo.infoClass.getConstructors.head // <-- NOTE: head here isn't bullet-proof, but a generally safe assumption for case classes.  (Req because of arg typing mess.)
    if (classInfo.typeMembers.nonEmpty) then
      val originalArgTypes = classInfo.fields.map(_.fieldType.infoClass)
      const.newInstance(args:_*).asInstanceOf[T]
    else
      const.newInstance(args:_*).asInstanceOf[T]

  def _read_createInstance(args: List[Object], foundBits: mutable.BitSet, captured: java.util.HashMap[String, _]): T = 
    val asBuilt = constructWith(args)
    if isSJCapture then
      asBuilt.asInstanceOf[SJCapture].captured = captured
    asBuilt

  def _read_updateFieldMembers( fmbn: Map[String, ClassFieldMember[_,_]]): ScalaClassTypeAdapter[T] = 
    this.copy(fieldMembersByName = fmbn)
