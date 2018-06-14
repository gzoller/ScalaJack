package co.blocke.scalajack
package csv

import java.lang.reflect.Method

import co.blocke.scalajack.csv.CSVCaseClassTypeAdapter.Member
import co.blocke.scalajack.typeadapter.OptionTypeAdapter

import scala.collection.immutable
import scala.language.{ existentials, reflectiveCalls }

object CSVCaseClassTypeAdapter extends TypeAdapterFactory.FromClassSymbol {

  trait Member[Owner] {

    type Value

    val index: Int
    val name: String
    val valueTypeAdapter: TypeAdapter[Value]
    val valueAccessorMethodSymbol: MethodSymbol
    val valueAccessorMethod: Method
    val derivedValueClassConstructorMirror: Option[MethodMirror]
    val outerClass: Option[java.lang.Class[_]]

    def isOptional: Boolean = valueTypeAdapter.isInstanceOf[OptionTypeAdapter[_]]

    def valueIn(taggedInstance: TypeTagged[Owner]): TypeTagged[Value] =
      taggedInstance match {
        case TypeTagged(instance) =>
          val value = valueAccessorMethod.invoke(instance)

          if (outerClass.isEmpty || outerClass.get.isInstance(value)) {
            TypeTagged.inferFromRuntimeClass[Value](value.asInstanceOf[Value])
          } else {
            derivedValueClassConstructorMirror match {
              case Some(methodMirror) =>
                TypeTagged.inferFromRuntimeClass[Value](methodMirror.apply(value).asInstanceOf[Value])

              case None =>
                TypeTagged.inferFromRuntimeClass[Value](value.asInstanceOf[Value])
            }
          }
      }

    def deserializeValueFromNothing[J](path: Path)(implicit ops: JsonOps[J]): DeserializationResult[Value] =
      valueTypeAdapter.deserializer.deserializeFromNothing(path)

    def deserializeValue[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[Value] =
      valueTypeAdapter.deserializer.deserialize(path, json)

    def serializeValue[J](tagged: TypeTagged[Value])(implicit ops: JsonOps[J]): SerializationResult[J] =
      valueTypeAdapter.serializer.serialize(tagged)

    def writeValue(parameterValue: Value, writer: Writer): Unit =
      valueTypeAdapter.write(parameterValue, writer)

  }

  override def typeAdapterOf[T](classSymbol: ClassSymbol, next: TypeAdapterFactory)(implicit context: Context, tt: TypeTag[T]): TypeAdapter[T] =
    if (classSymbol.isCaseClass) {
      val constructorSymbol = classSymbol.primaryConstructor.asMethod

      val classMirror = reflectClass(classSymbol)
      val constructorMirror = classMirror.reflectConstructor(constructorSymbol)

      val companionType: Type = classSymbol.companion.typeSignature
      val companionObject = reflectModule(classSymbol.companion.asModule).instance
      val companionMirror = reflect(companionObject)

      val members: IndexedSeq[Member[T]] = constructorSymbol.typeSignatureIn(tt.tpe).paramLists.flatten.zipWithIndex.map({
        case (member, index_) =>
          val memberName = member.name.encodedName.toString
          val accessorMethodSymbol = tt.tpe.member(TermName(memberName)).asMethod
          val accessorMethod = Reflection.methodToJava(accessorMethodSymbol)

          val (derivedValueClassConstructorMirror_, memberClass) =
            if (member.typeSignature.typeSymbol.isClass) {
              val memberClassSymbol = member.typeSignature.typeSymbol.asClass

              if (memberClassSymbol.isDerivedValueClass) {
                val memberClass = runtimeClass(memberClassSymbol)
                // The accessor will actually return the "inner" value, not the value class.
                val constructorMethodSymbol = memberClassSymbol.primaryConstructor.asMethod
                //              val innerClass = currentMirror.runtimeClass(constructorMethodSymbol.paramLists.flatten.head.info.typeSymbol.asClass)
                (Some(reflectClass(memberClassSymbol).reflectConstructor(constructorMethodSymbol)), Some(memberClass))
              } else {
                (None, None)
              }
            } else {
              (None, None)
            }

          val memberType = member.asTerm.typeSignature
          val memberTypeAdapter = context.typeAdapter(memberType)
          new Member[T] {
            override type Value = Any
            override val index: Int = index_
            override val name: String = memberName
            override val valueTypeAdapter: TypeAdapter[Value] = memberTypeAdapter.asInstanceOf[TypeAdapter[Value]]
            override val valueAccessorMethodSymbol: MethodSymbol = accessorMethodSymbol
            override val valueAccessorMethod: Method = accessorMethod
            override val derivedValueClassConstructorMirror: Option[MethodMirror] = derivedValueClassConstructorMirror_
            override val outerClass: Option[java.lang.Class[_]] = memberClass
          }
      }).to[immutable.IndexedSeq]

      CSVCaseClassTypeAdapter[T](
        new CSVCaseClassDeserializer[T](members, constructorMirror),
        new CSVCaseClassSerializer[T](members),
        tt.tpe, constructorMirror, tt.tpe, members).asInstanceOf[TypeAdapter[T]]
    } else {
      next.typeAdapterOf[T]
    }

}

case class CSVCaseClassTypeAdapter[T](
    override val deserializer: Deserializer[T],
    override val serializer:   Serializer[T],
    caseClassType:             Type,
    constructorMirror:         MethodMirror,
    tpe:                       Type,
    members:                   Seq[Member[T]]) extends TypeAdapter[T] {

  override def read(reader: Reader): T = {
    reader.peek match {
      case TokenType.Null =>
        reader.readNull().asInstanceOf[T]

      case TokenType.BeginObject =>
        reader.beginObject()
        val arguments = members.map(_.valueTypeAdapter.read(reader))
        reader.endObject()
        constructorMirror.apply(arguments: _*).asInstanceOf[T]
    }
  }

  override def write(value: T, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.beginObject()

      for (member <- members) {
        // We don't write member names for CSV
        member.writeValue(member.valueIn(TypeTagged.inferFromRuntimeClass[T](value)).get, writer)
      }
      writer.endObject()
    }

}
