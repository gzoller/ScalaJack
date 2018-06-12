package co.blocke

package object scalajack {

  type MemberName = String

  /*
  Scala Reflection API
   */
  type ClassSymbol = scala.reflect.runtime.universe.ClassSymbol
  type MethodSymbol = scala.reflect.runtime.universe.MethodSymbol
  type RuntimeClass = scala.reflect.runtime.universe.RuntimeClass
  type Symbol = scala.reflect.runtime.universe.Symbol
  type Type = scala.reflect.runtime.universe.Type
  type TypeSymbol = scala.reflect.runtime.universe.TypeSymbol
  type TypeTag[T] = scala.reflect.runtime.universe.TypeTag[T]
  type WeakTypeTag[T] = scala.reflect.runtime.universe.WeakTypeTag[T]

  @inline final def appliedType(tycon: Type, args: Type*): Type = scala.reflect.runtime.universe.appliedType(tycon, args: _*)
  @inline final def appliedType(tycon: Type, args: List[Type]): Type = scala.reflect.runtime.universe.appliedType(tycon, args)

  @inline final def classSymbol(rtcls: RuntimeClass): ClassSymbol = scala.reflect.runtime.currentMirror.classSymbol(rtcls)

  @inline final def staticClass(fullName: String): ClassSymbol = scala.reflect.runtime.currentMirror.staticClass(fullName)

  @inline final def symbolOf[T: WeakTypeTag]: TypeSymbol = scala.reflect.runtime.universe.symbolOf

  @inline final def typeOf[T: TypeTag]: Type = scala.reflect.runtime.universe.typeOf[T]

  @inline final val NoType = scala.reflect.runtime.universe.NoType

}
