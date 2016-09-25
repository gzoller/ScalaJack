package co.blocke.scalajack.bytecode

import scala.reflect.runtime
import org.objectweb.asm

object Type {

  val `java.lang.String` = typeOf[java.lang.String]
  val `java.lang.Object` = typeOf[java.lang.Object]

  val `java.lang.Byte` = typeOf[java.lang.Byte]
  val `java.lang.Character` = typeOf[java.lang.Character]
  val `java.lang.Double` = typeOf[java.lang.Double]
  val `java.lang.Float` = typeOf[java.lang.Float]
  val `java.lang.Integer` = typeOf[java.lang.Integer]
  val `java.lang.Long` = typeOf[java.lang.Long]
  val `java.lang.Short` = typeOf[java.lang.Short]
  val `java.lang.Boolean` = typeOf[java.lang.Boolean]

  def typeOf[T](implicit typeTag: runtime.universe.TypeTag[T]): Type = {
    typeOf(typeTag.tpe)
  }

  def typeOf[T](typeArguments: Type*)(implicit typeConstructorTag: runtime.universe.TypeTag[T]): Type = {
    ClassType(typeConstructorTag.tpe.typeConstructor.typeSymbol.fullName, typeArguments.map(_.boxed).toList)
  }

  def typeOf(tpe: runtime.universe.Type): Type = {
    import scala.reflect.runtime.currentMirror

    val asmType = asm.Type.getType(currentMirror.runtimeClass(tpe))

    asmType match {
      case asm.Type.VOID_TYPE ⇒ `void`
      case asm.Type.BYTE_TYPE ⇒ `byte`
      case asm.Type.CHAR_TYPE ⇒ `char`
      case asm.Type.DOUBLE_TYPE ⇒ `double`
      case asm.Type.FLOAT_TYPE ⇒ `float`
      case asm.Type.INT_TYPE ⇒ `int`
      case asm.Type.LONG_TYPE ⇒ `long`
      case asm.Type.SHORT_TYPE ⇒ `short`
      case asm.Type.BOOLEAN_TYPE ⇒ `boolean`
      case _ ⇒ ClassType(asmType.getClassName, tpe.typeArgs.map(typeOf(_)))
    }
  }

  case object `void` extends Type {

    override def isPrimitive = false

    override def asPrimitive = throw new UnsupportedOperationException("The void type is not a primitive type")

    override def isReference = false

    override def asReference = throw new UnsupportedOperationException("The void type is not a reference type")

    override def className = ???

    override def internalClassName = ???

    override def descriptor = "V"

    override def size = throw new UnsupportedOperationException("The void type does not have a size")

    override def boxed = throw new UnsupportedOperationException("The void type cannot be boxed")

    override def unboxed = throw new UnsupportedOperationException("The void type cannot be un-boxed")

    override def invocation(name: String, returnType: Type, parameterTypes: List[Type], isInterface: Boolean) =
      throw new UnsupportedOperationException("Cannot invoke methods on the void type")

    override def toString = "void"

    override def signature = "V"

    override def opcode(opcode: Int): Int = ???

  }

  case object `byte` extends PrimitiveType {

    override def toString = "byte"

    override def descriptor = "B"

    override def size = 1

    override def boxed = `java.lang.Byte`

    override def opcode(opcode: Int): Int =
      asm.Type.BYTE_TYPE.getOpcode(opcode)

  }

  case object `char` extends PrimitiveType {

    override def toString = "char"

    override def descriptor = "C"

    override def size = 1

    override def boxed = `java.lang.Character`

    override def opcode(opcode: Int): Int =
      asm.Type.CHAR_TYPE.getOpcode(opcode)

  }

  case object `double` extends PrimitiveType {

    override def toString = "double"

    override def descriptor = "D"

    override def size = 2

    override def boxed = `java.lang.Double`

    override def opcode(opcode: Int): Int =
      asm.Type.DOUBLE_TYPE.getOpcode(opcode)

  }

  case object `float` extends PrimitiveType {

    override def toString = "float"

    override def descriptor = "F"

    override def size = 1

    override def boxed = `java.lang.Float`

    override def opcode(opcode: Int): Int =
      asm.Type.FLOAT_TYPE.getOpcode(opcode)

  }

  case object `int` extends PrimitiveType {

    override def toString = "int"

    override def descriptor = "I"

    override def size = 1

    override def boxed = `java.lang.Integer`

    override def opcode(opcode: Int): Int =
      asm.Type.INT_TYPE.getOpcode(opcode)

  }

  case object `long` extends PrimitiveType {

    override def toString = "long"

    override def descriptor = "J"

    override def size = 2

    override def boxed = `java.lang.Long`

    override def opcode(opcode: Int): Int =
      asm.Type.LONG_TYPE.getOpcode(opcode)

  }

  case object `short` extends PrimitiveType {

    override def toString = "short"

    override def descriptor = "S"

    override def size = 1

    override def boxed = `java.lang.Short`

    override def opcode(opcode: Int): Int =
      asm.Type.SHORT_TYPE.getOpcode(opcode)

  }

  case object `boolean` extends PrimitiveType {

    override def toString = "boolean"

    override def descriptor = "Z"

    override def size = 1

    override def boxed = `java.lang.Boolean`

    override def opcode(opcode: Int): Int =
      asm.Type.BOOLEAN_TYPE.getOpcode(opcode)

  }

}

sealed trait Type {

  def isPrimitive: Boolean

  def asPrimitive: PrimitiveType

  def isReference: Boolean

  def asReference: ReferenceType

  def className: String

  def internalClassName: String

  def descriptor: String

  def signature: String

  def size: Int

  def boxed: Type

  def unboxed: Type

  override def toString = className

  def invocation(name: String, returnType: Type, parameterTypes: List[Type], isInterface: Boolean): Invocation

  def opcode(opcode: Int): Int

}

sealed trait PrimitiveType extends Type {

  override def isPrimitive = true

  override def asPrimitive = this

  override def isReference = false

  override def asReference = throw new UnsupportedOperationException(s"$this is not a primitive type")

  override def unboxed = this

  override def className = ???

  override def internalClassName = ???

  override def invocation(name: String, returnType: Type, parameterTypes: List[Type], isInterface: Boolean) =
    throw new UnsupportedOperationException("Cannot invoke methods on a primitive")

  override def signature = descriptor

}

sealed trait ReferenceType extends Type {

  override def isPrimitive = false

  override def asPrimitive = throw new UnsupportedOperationException(s"$this is not a reference type")

  override def isReference = true

  override def asReference = this

  override def size = 1

  override def boxed = this

  override def invocation(name: String, returnType: Type, parameterTypes: List[Type], isInterface: Boolean): Invocation =
    Invocation(this, name, returnType, parameterTypes, isInterface)

}

case class ClassType(className: String, typeArguments: List[Type]) extends ReferenceType {

  override def descriptor = s"L$internalClassName;"

  override def internalClassName = s"${className.replaceAllLiterally(".", "/")}"

  override def boxed: ReferenceType =
    ClassType(className, typeArguments.map(_.boxed))

  override def unboxed = {
    import Type._

    className match {
      case "java.lang.Byte" ⇒ `byte`
      case "java.lang.Character" ⇒ `char`
      case "java.lang.Double" ⇒ `double`
      case "java.lang.Float" ⇒ `float`
      case "java.lang.Integer" ⇒ `int`
      case "java.lang.Long" ⇒ `long`
      case "java.lang.Short" ⇒ `short`
      case "java.lang.Boolean" ⇒ `boolean`
      case _ ⇒ this
    }
  }

  override def toString =
    if (typeArguments.isEmpty) {
      className
    } else {
      s"$className<${typeArguments.map(_.toString).mkString(", ")}>"
    }

  override def signature: String =
    if (typeArguments.isEmpty) {
      s"L$internalClassName;"
    } else {
      s"L$internalClassName<${typeArguments.map(_.signature).mkString(", ")}>;"
    }

  override def opcode(opcode: Int): Int =
    asm.Type.getObjectType(internalClassName).getOpcode(opcode)

}

case class ArrayType(componentType: Type) extends ReferenceType {

  override def className = s"${componentType.className}[]"

  override def descriptor = s"[${componentType.descriptor}"

  override def unboxed = this

  override def internalClassName = ???

  override def signature: String = ???

  override def opcode(opcode: Int): Int = ???

}
