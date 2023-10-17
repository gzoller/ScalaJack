package co.blocke.scalajack
package typeadapter
package classes

import model._
import co.blocke.scala_reflection.info._
import co.blocke.scala_reflection._

import scala.collection.mutable


trait ScalaClassTypeAdapter[T](implicit taCache: TypeAdapterCache) extends ClassTypeAdapterBase[T]:
  val typeMembersByName:  Map[String, TypeMemberInfo]
    // dbCollectionName:   Option[String]

  private val classInfo = info.asInstanceOf[ClassInfo]

  val isSJCapture = classInfo.hasMixin(SJ_CAPTURE)

  def _read_createInstance(args: List[Object], foundBits: mutable.BitSet, captured: java.util.HashMap[String, _]): T
  def _read_updateFieldMembers( fmbn: Map[String, ClassFieldMember[_,_]]): ScalaClassTypeAdapter[T]

  def read(parser: Parser): T =
    if (parser.peekForNull) then
      null.asInstanceOf[T]
    else 
      // External type hint --> Substitute type field's type into the placeholder (i.e.'T') in the class' fields
      val (foundBits, args, captured) = {
        if (classInfo.typeMembers.nonEmpty) then
          val fixedFields = findActualTypeMemberTypes(parser)  // Resolve actual type of type member (should be a class) and substitute any fields having that type with the actual
          val substitutedClassInfo = _read_updateFieldMembers(fixedFields)
          parser.expectObject(substitutedClassInfo, taCache.jackFlavor.defaultHint)
        else
          parser.expectObject(this, taCache.jackFlavor.defaultHint)
      }

      val testBits = fieldBitsTemplate.collect{
        case b if !foundBits.contains(b) => b
      }
      if (testBits.isEmpty) then
        _read_createInstance(args, foundBits, captured)
      else
        parser.backspace()
        throw new ScalaJackError(
          parser.showError(
            s"Class ${classInfo.name} missing required fields: " + testBits
              .map(b => orderedFieldNames(b))
              .mkString(", ")
          )
        )


  def write[WIRE](
      t:      T,
      writer: Writer[WIRE],
      out:    mutable.Builder[WIRE, WIRE]): Unit = 

    // Resolve actual types (in t) of any type members
    val (allFields, filteredTypeMembers) = classInfo match {
      case c: ScalaCaseClassInfo => (classInfo.fields.toList, c.filterTraitTypeParams.typeMembers.toList)
      case c: ScalaClassInfo => (classInfo.fields.toList ++ c.nonConstructorFields.toList, c.filterTraitTypeParams.typeMembers.toList)
    }
    val (extras, resolvedFieldMembersByName) =
      if filteredTypeMembers.nonEmpty then
        val xtras: List[(String, ExtraFieldValue[_])] = filteredTypeMembers.map{ tm =>
          val foundActualField = allFields.find( _.asInstanceOf[ScalaFieldInfo].originalSymbol == Some(tm.typeSymbol) )
          val resolvedTypeMember = foundActualField.map{ a => 
            val actualRtype = RType.of(a.valueOf(t).getClass)
            tm.copy(memberType = actualRtype)
          }.getOrElse(tm)
          (
            resolvedTypeMember.name,
            ExtraFieldValue(
              taCache.jackFlavor.typeValueModifier.unapply(resolvedTypeMember.asInstanceOf[TypeMemberInfo].memberType.name),
              taCache.jackFlavor.stringTypeAdapter
            )
          )
        }

        val filteredTypeMemberSymbols = filteredTypeMembers.map(_.typeSymbol)
        val resolvedFields = fieldMembersByName.map{ case (fname, aField) =>        
          val aScalaField = aField.info.asInstanceOf[ScalaFieldInfo]
          if aScalaField.originalSymbol.isDefined && filteredTypeMemberSymbols.contains(aScalaField.originalSymbol.get) then
            val actualRtype = RType.of(aScalaField.valueOf(t).getClass)
            fname -> aField.copy( info = aScalaField.copy( fieldType = actualRtype ), valueTypeAdapter = taCache.typeAdapterOf(actualRtype) )
          else
            fname -> aField
        }

        (xtras, resolvedFields)
      else
        (Nil, fieldMembersByName)

    writer.writeObject(
      t,
      orderedFieldNames,
      resolvedFieldMembersByName,
      out,
      extras
    )

  // Used by AnyTypeAdapter to insert type hint (not normally needed) into output so object
  // may be reconstituted on read
  def writeWithHint[WIRE](
      jackFlavor: JackFlavor[WIRE],
      t:          T,
      writer:     Writer[WIRE],
      out:        mutable.Builder[WIRE, WIRE]): Unit = 
    val hintValue = t.getClass.getName
    val hintLabel = jackFlavor.getHintLabelFor(info)
    val extra = List(
      (
        hintLabel,
        ExtraFieldValue(hintValue, jackFlavor.stringTypeAdapter)
      )
    )
    writer.writeObject(t, orderedFieldNames, fieldMembersByName, out, extra)

  // Use parser to scan JSON for type member name and materialize the TypeMemberInfo with RType of actual/found type.
  // (The original TypeMember's RType is likely a trait.  The parser-found type should be a concrete class (ScalaCaseClassInfo).)
  private def findActualTypeMemberTypes(
      parser:  Parser
  ): Map[String, ClassFieldMember[_,_]] = 
    val foundByParser: Map[String, TypeMemberInfo] = parser.resolveTypeMembers(
      typeMembersByName,
      taCache.jackFlavor.typeValueModifier
    )
    // Filter any non-trait/class type members... we ignore these so they don't mess up type hint modifiers
    val filtered = typeMembersByName.collect {
      case (k,tm) if tm.memberType.isInstanceOf[TraitInfo] || tm.memberType.isInstanceOf[ScalaCaseClassInfo] => (k,tm)
    }
    if (filtered.size != foundByParser.size)
      throw new ScalaJackError(
        parser.showError(
          "Did not find required type member(s): " + typeMembersByName.keySet
            .diff(foundByParser.keySet.map(_.toString))
            .mkString(",")
        )
      )

    // Map[TypeSymbol,TypeSymbolInfo] 
    val invertedBySymbol = foundByParser.map( (name, tpeInfo) => (tpeInfo.typeSymbol, tpeInfo))

    // NOTE: This is sub-optimal and not "deep".  Need a better solution to sew past Level-1 for a general n-deep solution
    // As it stands, reflection doesn't provide a way to "sew" types deeply (dynamically).
    fieldMembersByName.map {
      case (name, fm) if fm.info.originalSymbol.flatMap(s => invertedBySymbol.get(s)).isDefined =>
        val actualTypeAdapter = taCache.typeAdapterOf(invertedBySymbol(fm.info.originalSymbol.get).memberType) // get TypeAdapter for actual type
        val fixedTypeAdapter = fm.valueTypeAdapter match {
          case fallback: FallbackTypeAdapter[_, _] =>
            FallbackTypeAdapter(
              actualTypeAdapter.asInstanceOf[TypeAdapter[Any]],
              fallback.orElseTypeAdapter.asInstanceOf[TypeAdapter[Any]]
            )
          case _ => actualTypeAdapter
        }
        (name, fm.copy(info = fm.info.asInstanceOf[ScalaFieldInfo].copy(fieldType = invertedBySymbol(fm.info.originalSymbol.get)), valueTypeAdapter = fixedTypeAdapter))
      case (name, fm) =>
        (name, fm)
    }

