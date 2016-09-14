package co.blocke.scalajack.json
package typeadapter

import java.lang.reflect.Method
import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{ ClassSymbol, MethodMirror, MethodSymbol, TermName, Type }

object DerivedValueClassAdapter extends TypeAdapterFactory.FromClassSymbol {

  override def typeAdapter(tpe: Type, classSymbol: ClassSymbol, context: Context): Option[TypeAdapter[_]] =
    if (classSymbol.isDerivedValueClass) {
      val constructorSymbol = classSymbol.primaryConstructor.asMethod
      val constructorMirror = currentMirror.reflectClass(classSymbol).reflectConstructor(constructorSymbol)

      val parameter = constructorSymbol.paramLists.head.head
      val parameterName = parameter.name.encodedName.toString
      val accessorMethodSymbol = tpe.member(TermName(parameterName)).asMethod
      val accessorMethod = Reflection.methodToJava(accessorMethodSymbol)

      val valueType = parameter.infoIn(tpe).substituteTypes(tpe.typeConstructor.typeParams, tpe.typeArgs)
      val valueTypeAdapter = context.typeAdapter(valueType)

      Some(DerivedValueClassAdapter(constructorMirror, accessorMethodSymbol, accessorMethod, valueTypeAdapter))
    } else {
      None
    }

}

case class DerivedValueClassAdapter[DerivedValueClass, Value](
    constructorMirror:    MethodMirror,
    accessorMethodSymbol: MethodSymbol,
    accessorMethod:       Method,
    valueTypeAdapter:     TypeAdapter[Value]
) extends TypeAdapter[DerivedValueClass] {

  override def read(reader: Reader): DerivedValueClass = {
    val value = valueTypeAdapter.read(reader)
    constructorMirror.apply(value).asInstanceOf[DerivedValueClass]
  }

  override def write(value: DerivedValueClass, writer: Writer): Unit = {
    val wrappedValue = accessorMethod.invoke(value).asInstanceOf[Value]
    valueTypeAdapter.write(wrappedValue.asInstanceOf[Value], writer)
  }

}
