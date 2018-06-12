package co.blocke

import scala.reflect.ClassTag

package object scalajack {

  type MemberName = String

  /*
  Scala Reflection API
   */
  type ClassMirror = scala.reflect.runtime.universe.ClassMirror
  type ClassSymbol = scala.reflect.runtime.universe.ClassSymbol
  type InstanceMirror = scala.reflect.runtime.universe.InstanceMirror
  type MethodMirror = scala.reflect.runtime.universe.MethodMirror
  type MethodSymbol = scala.reflect.runtime.universe.MethodSymbol
  type ModuleMirror = scala.reflect.runtime.universe.ModuleMirror
  type ModuleSymbol = scala.reflect.runtime.universe.ModuleSymbol
  type Mirror = scala.reflect.runtime.universe.Mirror
  type RuntimeClass = scala.reflect.runtime.universe.RuntimeClass
  type Symbol = scala.reflect.runtime.universe.Symbol
  type Type = scala.reflect.runtime.universe.Type
  type TypeSymbol = scala.reflect.runtime.universe.TypeSymbol
  type TypeTag[T] = scala.reflect.runtime.universe.TypeTag[T]
  type WeakTypeTag[T] = scala.reflect.runtime.universe.WeakTypeTag[T]

  @inline final val TermName = scala.reflect.runtime.universe.TermName

  @inline final def appliedType(tycon: Type, args: Type*): Type = scala.reflect.runtime.universe.appliedType(tycon, args: _*)
  @inline final def appliedType(tycon: Type, args: List[Type]): Type = scala.reflect.runtime.universe.appliedType(tycon, args)

  @inline final def classSymbol(rtcls: RuntimeClass): ClassSymbol = scala.reflect.runtime.currentMirror.classSymbol(rtcls)

  @inline final def reflect[T: ClassTag](obj: T): InstanceMirror = scala.reflect.runtime.currentMirror.reflect[T](obj)
  @inline final def reflectClass(cls: ClassSymbol): ClassMirror = scala.reflect.runtime.currentMirror.reflectClass(cls)
  @inline final def reflectModule(mod: ModuleSymbol): ModuleMirror = scala.reflect.runtime.currentMirror.reflectModule(mod)

  @inline final def runtimeClass(tpe: Type): java.lang.Class[_] = scala.reflect.runtime.currentMirror.runtimeClass(tpe)
  @inline final def runtimeClassOf[T: TypeTag]: java.lang.Class[T] = scala.reflect.runtime.currentMirror.runtimeClass(implicitly[TypeTag[T]].tpe).asInstanceOf[java.lang.Class[T]]

  @inline final def staticClass(fullName: String): ClassSymbol = scala.reflect.runtime.currentMirror.staticClass(fullName)

  @inline final def symbolOf[T: WeakTypeTag]: TypeSymbol = scala.reflect.runtime.universe.symbolOf

  @inline final def typeOf[T: TypeTag]: Type = scala.reflect.runtime.universe.typeOf[T]

  @inline final val NoType = scala.reflect.runtime.universe.NoType

  implicit val typeTagType: TypeTag[Type] = TypeTags.of[Type](TypeTagHacks.TypeType)

}
