package co.blocke

import co.blocke.series60.util._
import reflect.runtime.universe._
import reflect.ClassTag

// NOTE: Some items here are commented out because they don't seem to be used.

package object series60 {

  type MemberName = String

  /*
  Scala Reflection API
   */
  // format: OFF
  type AST_PRIMITIVE   = Any
  type ClassMirror     = scala.reflect.runtime.universe.ClassMirror
  type ClassSymbol     = scala.reflect.runtime.universe.ClassSymbol
  type ConstantApi     = scala.reflect.runtime.universe.ConstantApi
  type ConstantTypeApi = scala.reflect.runtime.universe.ConstantTypeApi
  type InstanceMirror  = scala.reflect.runtime.universe.InstanceMirror
  type MethodMirror    = scala.reflect.runtime.universe.MethodMirror
  type MethodSymbol    = scala.reflect.runtime.universe.MethodSymbol
  type ModuleMirror    = scala.reflect.runtime.universe.ModuleMirror
  type ModuleSymbol    = scala.reflect.runtime.universe.ModuleSymbol
  type Mirror          = scala.reflect.runtime.universe.Mirror
  type RuntimeClass    = scala.reflect.runtime.universe.RuntimeClass
  type SingleTypeApi   = scala.reflect.runtime.universe.SingleTypeApi
  type Symbol          = scala.reflect.runtime.universe.Symbol
  type ThisTypeApi     = scala.reflect.runtime.universe.ThisTypeApi
  type Type            = scala.reflect.runtime.universe.Type
  type TypeRefApi      = scala.reflect.runtime.universe.TypeRefApi
  type TypeSymbol      = scala.reflect.runtime.universe.TypeSymbol
  type TypeTag[T]      = scala.reflect.runtime.universe.TypeTag[T]
  type WeakTypeTag[T]  = scala.reflect.runtime.universe.WeakTypeTag[T]
  // format: ON

  @inline final val TermName = scala.reflect.runtime.universe.TermName
  @inline final val NoType = scala.reflect.runtime.universe.NoType

  // format: OFF
  @inline final def appliedType(tycon: Type, args: Type*): Type        = scala.reflect.runtime.universe.appliedType(tycon, args: _*)
  @inline final def appliedType(tycon: Type, args: List[Type]): Type   = scala.reflect.runtime.universe.appliedType(tycon, args)
  //  @inline final def classSymbol(rtcls: RuntimeClass): ClassSymbol      = scala.reflect.runtime.currentMirror.classSymbol(rtcls)
  @inline final def reflect[T: ClassTag](obj: T): InstanceMirror       = scala.reflect.runtime.currentMirror.reflect[T](obj)
  @inline final def reflectClass(cls: ClassSymbol): ClassMirror        = scala.reflect.runtime.currentMirror.reflectClass(cls)
  @inline final def reflectModule(mod: ModuleSymbol): ModuleMirror     = scala.reflect.runtime.currentMirror.reflectModule(mod)
  //  @inline final def runtimeClass(sym: ClassSymbol): java.lang.Class[_] = scala.reflect.runtime.currentMirror.runtimeClass(sym)
  @inline final def runtimeClass(tpe: Type): java.lang.Class[_]        = scala.reflect.runtime.currentMirror.runtimeClass(tpe)
  //  @inline final def runtimeClassOf[T: TypeTag]: java.lang.Class[T]     = scala.reflect.runtime.currentMirror.runtimeClass(implicitly[TypeTag[T]].tpe).asInstanceOf[java.lang.Class[T]]
  @inline final def staticClass(fullName: String): ClassSymbol         = scala.reflect.runtime.currentMirror.staticClass(fullName)
  //  @inline final def symbolOf[T: WeakTypeTag]: TypeSymbol               = scala.reflect.runtime.universe.symbolOf[T]
  @inline final def typeOf[T: TypeTag]: Type                           = scala.reflect.runtime.universe.typeOf[T]
  // format: ON

  implicit val typeTagType: TypeTag[Type] = TypeTags.of[Type](TypeTagHacks.TypeType)

  @inline final def typeFromClassName(className: String) = staticClass(className).toType

  def typeToClassTag[T: TypeTag]: ClassTag[T] = {
    ClassTag[T](typeTag[T].mirror.runtimeClass(typeTag[T].tpe))
  }

  /*
  // Type Extractors
  object SingleType {
    def apply(tpe: Type, sym: Symbol): Type =
      scala.reflect.runtime.universe.internal.singleType(tpe, sym).typeSymbol.asType.toType
  }
  */
}

