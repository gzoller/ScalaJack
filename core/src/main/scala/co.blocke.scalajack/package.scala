package co.blocke

import co.blocke.scalajack.util.BijectiveFunction

import reflect.runtime.universe._
import reflect.ClassTag
import scala.reflect.runtime.universe

package object scalajack {
  type JSON          = String
  type DELIMITED     = String
  type YAML          = String
  type MemberName    = String
  type HintBijective = BijectiveFunction[String, Type]

  val DELIM_PREFIX: Char = 2

  @inline final def reflect[T: ClassTag](obj: T): InstanceMirror =
    scala.reflect.runtime.currentMirror.reflect[T](obj)
  @inline final def reflectModule(mod: ModuleSymbol): ModuleMirror =
    scala.reflect.runtime.currentMirror.reflectModule(mod)
  @inline final def staticClass(fullName: String): ClassSymbol =
    scala.reflect.runtime.currentMirror.staticClass(fullName)
  @inline final def runtimeClass(tpe: Type): java.lang.Class[_] =
    scala.reflect.runtime.currentMirror.runtimeClass(tpe)
  @inline final def reflectClass(cls: ClassSymbol): ClassMirror =
    scala.reflect.runtime.currentMirror.reflectClass(cls)
  @inline final def typeFromClassName(className: String): universe.Type =
    staticClass(className).toType
  @inline final def typeToClassTag[T: TypeTag]: ClassTag[T] =
    ClassTag[T](typeTag[T].mirror.runtimeClass(typeTag[T].tpe))
}
