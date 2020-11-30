package co.blocke.scalajack
package typeadapter
package classes

import co.blocke.scala_reflection.info._
import co.blocke.scala_reflection._
import model._
import scala.collection.mutable
import java.lang.reflect.Constructor

object ScalaClassTypeAdapterFactory extends TypeAdapterFactory:
  
  def matches(concrete: RType): Boolean = 
    concrete match {
      case c: ScalaCaseClassInfo if !c.isValueClass => true
      case c: ScalaClassInfo => true
      case _ => false
    }

  final val CLASSCLASS = Class.forName("java.lang.Class")

  inline def bakeFieldMembersByName( 
      fields: List[FieldInfo], 
      constructor: Constructor[_], 
      infoClass: Class[_] )(implicit taCache: TypeAdapterCache
    ): (Map[String,ClassFieldMember[_,_]], mutable.BitSet, Array[Object], List[String])  =

    // Filter out any ignored fields and re-index them all
    val fieldsWeCareAbout = fields.filterNot(_.annotations.contains(IGNORE)).zipWithIndex.map{ (f,idx) => f.reIndex(idx) }
    
    val bits = mutable.BitSet()
    val args = new Array[Object](fieldsWeCareAbout.size)

    val fieldsByName = fieldsWeCareAbout.map { f =>
      val fieldTypeAdapter = f.fieldType match {
        case _: TypeSymbolInfo => taCache.typeAdapterOf(impl.PrimitiveType.Scala_Any) // Any unresolved type symbols must be considered Any
        case t =>
          taCache.typeAdapterOf(t) match {
            // In certain situations, value classes need to be unwrapped, i.e. use the type adapter of their member.
            case vta: ValueClassTypeAdapter[_,_] if f.index < constructor.getParameterTypes().size =>   // value class in constructor
              val constructorParamClass = constructor.getParameterTypes()(f.index).getClass
              if constructorParamClass == vta.info.infoClass || constructorParamClass == CLASSCLASS then
                vta
              else
                vta.elementTypeAdapter
            case vta: ValueClassTypeAdapter[_,_] =>   // value class as body member
              val returnTypeClass = infoClass.getMethod(f.name).getReturnType
              if returnTypeClass == vta.info.infoClass || returnTypeClass == CLASSCLASS then
                vta
              else
                vta.elementTypeAdapter
            case other => 
              other
          }
      }

      // See if there's a default value set and blip bits/args accordingly to "pre-set" these values
      if f.defaultValue.isDefined then
        args(f.index) = f.defaultValue.get
      else if fieldTypeAdapter.defaultValue.isDefined then
        args(f.index) = fieldTypeAdapter.defaultValue.get.asInstanceOf[Object]
      else
        bits += f.index 

      val fieldMapName = f.annotations.get(CHANGE_ANNO).map(_("name"))
      val typeAdapter = fieldMapName.getOrElse(f.name) -> ClassFieldMember(
        f,
        fieldTypeAdapter,
        infoClass,
        f.annotations.get(DB_KEY).map(_.getOrElse("index","0").toInt),
        fieldMapName
      )
      // If this field is optional, we need to set the bits accordingly
      if typeAdapter._2.isOptional then
        bits -= f.index
      typeAdapter
    }.toMap
    (fieldsByName, bits, args, fieldsWeCareAbout.map( f => f.annotations.get(CHANGE_ANNO).map(_("name")).getOrElse(f.name) ))
  
  def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[_] =
    concrete match {
      case classInfo: ScalaCaseClassInfo =>
        val (fieldMembersByName, bits, args, orderedFieldNames) = bakeFieldMembersByName(classInfo.fields.toList, classInfo.constructor, classInfo.infoClass)
        CaseClassTypeAdapter(
          concrete,
          fieldMembersByName,
          args,
          bits,
          classInfo.typeMembers.map( tmem => (tmem.name, tmem.asInstanceOf[TypeMemberInfo]) ).toMap,
          orderedFieldNames,
          classInfo.annotations.get(DB_COLLECTION).map(_("name")) // Exctract Collection name annotation if present (for plain classes)
        )

      case classInfo: ScalaClassInfo =>
        // Nullify the defaultValue getter for non-constructor fields if there's no @Optional annotation
        val nonConstructorFields = classInfo.nonConstructorFields.filterNot(_.name == "captured").map{ _ match {
            case f if f.annotations.contains(OPTIONAL_ANNO) => f
            case f: ScalaFieldInfo if f.fieldType.isInstanceOf[OptionInfo] => f
            case f: ScalaFieldInfo => f.copy(defaultValueAccessorName = None)
          }
        }

        val (fieldMembersByName, bits, args, orderedFieldNames) = 
          bakeFieldMembersByName(classInfo.fields.toList ++ nonConstructorFields, classInfo.constructor, classInfo.infoClass)
        val paramSize = classInfo.constructor.getParameterTypes().size
        NonCaseClassTypeAdapter(
          concrete,
          fieldMembersByName,
          args,
          bits,
          classInfo.typeMembers.map( tmem => (tmem.name, tmem.asInstanceOf[TypeMemberInfo]) ).toMap,
          orderedFieldNames,
          fieldMembersByName.values.filter(_.info.asInstanceOf[ScalaFieldInfo].isNonConstructorField).toList,
          classInfo.annotations.get(DB_COLLECTION).map(_("name")) // Exctract Collection name annotation if present (for plain classes)
        )
    }
