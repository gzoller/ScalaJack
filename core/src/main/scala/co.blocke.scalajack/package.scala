package co.blocke

import scala.reflect.ClassTag

package object scalajack {

  type MemberName = String

  /*
  Scala Reflection API
   */
  // format: OFF
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

  final val No_Quote_Marker = '\u00C5'

  // format: OFF
  @inline final def appliedType(tycon: Type, args: Type*): Type        = scala.reflect.runtime.universe.appliedType(tycon, args: _*)
  @inline final def appliedType(tycon: Type, args: List[Type]): Type   = scala.reflect.runtime.universe.appliedType(tycon, args)
  @inline final def classSymbol(rtcls: RuntimeClass): ClassSymbol      = scala.reflect.runtime.currentMirror.classSymbol(rtcls)
  @inline final def reflect[T: ClassTag](obj: T): InstanceMirror       = scala.reflect.runtime.currentMirror.reflect[T](obj)
  @inline final def reflectClass(cls: ClassSymbol): ClassMirror        = scala.reflect.runtime.currentMirror.reflectClass(cls)
  @inline final def reflectModule(mod: ModuleSymbol): ModuleMirror     = scala.reflect.runtime.currentMirror.reflectModule(mod)
  @inline final def runtimeClass(sym: ClassSymbol): java.lang.Class[_] = scala.reflect.runtime.currentMirror.runtimeClass(sym)
  @inline final def runtimeClass(tpe: Type): java.lang.Class[_]        = scala.reflect.runtime.currentMirror.runtimeClass(tpe)
  @inline final def runtimeClassOf[T: TypeTag]: java.lang.Class[T]     = scala.reflect.runtime.currentMirror.runtimeClass(implicitly[TypeTag[T]].tpe).asInstanceOf[java.lang.Class[T]]
  @inline final def staticClass(fullName: String): ClassSymbol         = scala.reflect.runtime.currentMirror.staticClass(fullName)
  @inline final def symbolOf[T: WeakTypeTag]: TypeSymbol               = scala.reflect.runtime.universe.symbolOf[T]
  @inline final def typeOf[T: TypeTag]: Type                           = scala.reflect.runtime.universe.typeOf[T]
  // format: ON

  implicit val typeTagType: TypeTag[Type] = TypeTags.of[Type](TypeTagHacks.TypeType)

  // Type Extractors
  object SingleType {

    //    @deprecated(message = "Used only for documentation purposes", since = "v7")
    def apply(tpe: Type, sym: Symbol): Type =
      scala.reflect.runtime.universe.internal.singleType(tpe, sym).typeSymbol.asType.toType

    def unapply(tpe: Type): Option[(Type, Symbol)] =
      tpe match {
        case singleType: SingleTypeApi =>
          Some((singleType.pre, singleType.sym))

        case _ =>
          None
      }

  }

  object ThisType {

    @deprecated(message = "Used only for documentation purposes", since = "v7")
    def apply(sym: Symbol): ThisTypeApi = ???

    def unapply(tpe: Type): Option[Symbol] =
      tpe match {
        case thisType: ThisTypeApi =>
          Some(thisType.sym)

        case _ =>
          None
      }

  }

  object TypeRef {

    @deprecated(message = "Used only for documentation purposes", since = "v7")
    def apply(pre: Type, sym: Symbol, args: List[Type]): TypeRefApi = ???

    def unapply(tpe: Type): Option[(Type, Symbol, List[Type])] =
      tpe match {
        case typeRef: TypeRefApi =>
          Some((typeRef.pre, typeRef.sym, typeRef.args))

        case _ =>
          None
      }

  }

}
