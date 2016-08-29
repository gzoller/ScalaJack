package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{Context, Reader, TypeAdapter, TypeAdapterFactory, Writer}

import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{ClassSymbol, MethodMirror, MethodSymbol, TermName, Type}
import scala.reflect.ClassTag

object DerivedValueClassAdapter extends TypeAdapterFactory.FromClassSymbol {

  override def typeAdapter(tpe: Type, classSymbol: ClassSymbol, context: Context): Option[TypeAdapter[_]] =
    if (classSymbol.isDerivedValueClass) {
      val constructorSymbol = classSymbol.primaryConstructor.asMethod

      val constructorMirror = currentMirror.reflectClass(classSymbol).reflectConstructor(constructorSymbol)

      val parameter = constructorSymbol.paramLists.head.head
      val parameterName = parameter.name.encodedName.toString
      val accessor = tpe.member(TermName(parameterName)).asMethod

      val valueType = parameter.infoIn(tpe).substituteTypes(tpe.typeConstructor.typeParams, tpe.typeArgs)
      val valueTypeAdapter = context.typeAdapter(valueType)

      Some(DerivedValueClassAdapter(constructorMirror, accessor, valueTypeAdapter))
    } else {
      None
    }

}

case class DerivedValueClassAdapter[DerivedValueClass, Value](constructorMirror: MethodMirror,
                                                              accessor: MethodSymbol,
                                                              valueTypeAdapter: TypeAdapter[Value]) extends TypeAdapter[DerivedValueClass] {

  override def read(reader: Reader): DerivedValueClass = {
    val value = valueTypeAdapter.read(reader)
    constructorMirror.apply(value).asInstanceOf[DerivedValueClass]
  }

  override def write(value: DerivedValueClass, writer: Writer): Unit = {
    val v = currentMirror.reflect(value)(ClassTag(value.getClass)).reflectMethod(accessor).apply().asInstanceOf[Value]
    valueTypeAdapter.write(v, writer)
  }


}
