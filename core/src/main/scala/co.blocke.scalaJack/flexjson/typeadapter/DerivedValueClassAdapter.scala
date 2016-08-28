package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.flexjson.{Context, Reader, TypeAdapter, TypeAdapterFactory, Writer}

import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{MethodMirror, Type}

object DerivedValueClassAdapter extends TypeAdapterFactory {

  override def typeAdapter(tpe: Type, context: Context): Option[TypeAdapter[_]] = {
    if (tpe.typeSymbol.isClass) {
      val classSymbol = tpe.typeSymbol.asClass
      if (classSymbol.isDerivedValueClass) {
        val constructorSymbol = classSymbol.primaryConstructor.asMethod

        val constructorMirror = currentMirror.reflectClass(classSymbol).reflectConstructor(constructorSymbol)

        val valueType = constructorSymbol.paramLists.head.head.infoIn(tpe).substituteTypes(tpe.typeConstructor.typeParams, tpe.typeArgs)
        val valueTypeAdapter = context.typeAdapter(valueType)

        Some(DerivedValueClassAdapter(constructorMirror, valueTypeAdapter))
      } else {
        None
      }
    } else {
      None
    }
  }

}

case class DerivedValueClassAdapter[DerivedValueClass, Value](constructorMirror: MethodMirror,
                                                              valueTypeAdapter: TypeAdapter[Value]) extends TypeAdapter[DerivedValueClass] {

  override def read(reader: Reader): DerivedValueClass = {
    val value = valueTypeAdapter.read(reader)
    constructorMirror.apply(value).asInstanceOf[DerivedValueClass]
  }

  override def write(value: DerivedValueClass, writer: Writer): Unit = ???

}
