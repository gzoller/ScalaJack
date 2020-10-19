package co.blocke.scalajack
package typeadapter
package classes

import model._

import scala.collection.mutable
import co.blocke.scala_reflection._
import co.blocke.scala_reflection.info._
import scala.util.Try

object JavaClassTypeAdapterFactory extends TypeAdapterFactory:
  def matches(concrete: RType): Boolean = 
    concrete match {
      case _: JavaClassInfo => true
      case _ => false
    }

  def makeTypeAdapter(concrete: RType)(implicit taCache: TypeAdapterCache): TypeAdapter[_] =
    val classInfo = concrete.asInstanceOf[ClassInfo]

    // Filter out any ignored fields and re-index them all
    val fieldsWeCareAbout = classInfo.fields.filterNot(_.annotations.contains(IGNORE)).zipWithIndex.map{ (f,idx) => f.reIndex(idx) }

    val bits = mutable.BitSet()
    val args = new Array[Object](fieldsWeCareAbout.size)

    Try(classInfo.asInstanceOf[JavaClassInfo].infoClass.getConstructor()).toOption.orElse(
      throw new ScalaJackError("ScalaJack does not support Java classes with a non-empty constructor.")
    )

    val const = classInfo.asInstanceOf[JavaClassInfo].infoClass.getConstructors.head
    val phantomInstance = const.newInstance()  // used to get default/initially-set values 

    val fieldMembersByName = 
      fieldsWeCareAbout.map { f =>
        f.fieldType match {
          case c: TypeSymbolInfo => throw new ScalaJackError(s"Concrete type expected for class ${concrete.name} field ${f.name}.  ${c.getClass.getName} was found.")
          case c =>
            bits += f.index
            val fieldMapName = f.annotations.get(CHANGE_ANNO).map(_("name")) 
            val classFieldMember = ClassFieldMember(
              f,
              taCache.typeAdapterOf(c),
              classInfo.infoClass, 
              f.annotations.get(DB_KEY).map(_.getOrElse("index","0").toInt),
              fieldMapName
            )
            if classFieldMember.isOptional then // filter out @Optional annotated fields
              bits -= f.index
              args(f.index) = f.asInstanceOf[JavaFieldInfo].valueAccessor.invoke(phantomInstance)
            fieldMapName.getOrElse(f.name) -> classFieldMember
        }
      }.toMap

    // Exctract Collection name annotation if present (for plain classes)
    val dbCollectionAnnotation = classInfo.annotations.get(DB_COLLECTION).map(_("name"))

    JavaClassTypeAdapter(
      concrete, 
      args, 
      bits, 
      fieldMembersByName, 
      fieldsWeCareAbout.map( f => f.annotations.get(CHANGE_ANNO).map(_("name")).getOrElse(f.name)).toList,
      dbCollectionAnnotation
      )


case class JavaClassTypeAdapter[J](
    info:               RType,
    argsTemplate:       Array[Object],
    fieldBitsTemplate:  mutable.BitSet,
    fieldMembersByName: Map[String, ClassFieldMember[_,_]],
    orderedFieldNames:  List[String],
    dbCollectionName:   Option[String]
  )(implicit taCache: TypeAdapterCache) extends ClassTypeAdapterBase[J]:

  val javaClassInfo = info.asInstanceOf[JavaClassInfo]
  val isSJCapture = javaClassInfo.hasMixin(SJ_CAPTURE)

  def read(parser: Parser): J =
    val (foundBits, args, captured) = parser.expectObject(
      this,
      taCache.jackFlavor.defaultHint
    )
    val testBits = fieldBitsTemplate.collect{
      case b if !foundBits.contains(b) => b
    }
    if (testBits.isEmpty) then
      val const = javaClassInfo.infoClass.getConstructors.head
      val asBuilt = const.newInstance().asInstanceOf[J]
      if isSJCapture then
        asBuilt.asInstanceOf[SJCapture].captured = captured
      fieldMembersByName.values.map( f => f.info.asInstanceOf[JavaFieldInfo].valueSetter.invoke(asBuilt, args(f.info.index)) )
      asBuilt
    else
      parser.backspace()
      throw new ScalaJackError(
        parser.showError(
          s"Class ${info.name} missing required fields: " + testBits
            .map(b => orderedFieldNames(b))
            .mkString(", ")
        )
      )
        
  def write[WIRE](
      t:      J,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit =
    val extras = scala.collection.mutable.ListBuffer.empty[(String, ExtraFieldValue[_])]
    writer.writeObject(
      t,
      orderedFieldNames,
      fieldMembersByName,
      out,
      extras.toList)