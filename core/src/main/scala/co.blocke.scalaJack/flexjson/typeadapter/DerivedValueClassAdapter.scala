package co.blocke.scalajack.flexjson.typeadapter

import co.blocke.scalajack.{ CustomReadRender, ValueClassCustom }
import co.blocke.scalajack.flexjson.{ BijectiveFunction, Context, Reader, TypeAdapter, TypeAdapterFactory, Writer }
import co.blocke.scalajack.json.JsonKind

import scala.reflect.runtime.currentMirror
import scala.reflect.runtime.universe.{ ClassSymbol, MethodMirror, MethodSymbol, TermName, Type }
import scala.reflect.ClassTag

object DerivedValueClassAdapter extends TypeAdapterFactory.FromClassSymbol {

  override def typeAdapter(tpe: Type, classSymbol: ClassSymbol, context: Context): Option[TypeAdapter[_]] =
    if (classSymbol.isDerivedValueClass) {
      val constructorSymbol = classSymbol.primaryConstructor.asMethod
      val constructorMirror = currentMirror.reflectClass(classSymbol).reflectConstructor(constructorSymbol)

      val companionMirror = currentMirror.reflectModule(classSymbol.companion.asModule)
      val companion = companionMirror.instance

      val parameter = constructorSymbol.paramLists.head.head
      val parameterName = parameter.name.encodedName.toString
      val accessor = tpe.member(TermName(parameterName)).asMethod

      val valueTypeAdapter =
        companion match {
          case valueClassCustom: ValueClassCustom ⇒
            val anyTypeAdapter = context.typeAdapterOf[Any]
            val kind = JsonKind()

            val f: BijectiveFunction[Any, Any] = {
              import co.blocke.scalajack.flexjson.BijectiveFunction.Implicits._

              val apply = (jsonValue: Any) ⇒ valueClassCustom.read((kind, jsonValue))
              val unapply = (scalaValue: Any) ⇒ valueClassCustom.render((kind, scalaValue))

              apply ⇄ unapply
            }

            anyTypeAdapter andThen f

          case _ ⇒
            val valueType = parameter.infoIn(tpe).substituteTypes(tpe.typeConstructor.typeParams, tpe.typeArgs)
            context.typeAdapter(valueType)
        }

      Some(DerivedValueClassAdapter(constructorMirror, accessor, valueTypeAdapter))
    } else {
      None
    }

}

case class DerivedValueClassAdapter[DerivedValueClass, Value](
    constructorMirror: MethodMirror,
    accessor:          MethodSymbol,
    valueTypeAdapter:  TypeAdapter[Value]
) extends TypeAdapter[DerivedValueClass] {

  override def read(reader: Reader): DerivedValueClass = {
    val value = valueTypeAdapter.read(reader)
    constructorMirror.apply(value).asInstanceOf[DerivedValueClass]
  }

  override def write(value: DerivedValueClass, writer: Writer): Unit = {
    val wrappedValue = currentMirror.reflect(value)(ClassTag(value.getClass)).reflectMethod(accessor).apply().asInstanceOf[Value]
    valueTypeAdapter.write(wrappedValue, writer)
  }

}
