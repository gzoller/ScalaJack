package co.blocke.scalajack.bytecode

import org.objectweb.asm
import asm.{Label, MethodVisitor}
import asm.Opcodes._
import Type._

class MethodGenerator(ownerType: Type, mv: MethodVisitor) {

  var nextLocalVariableIndex = 0
  var localVariables = List[LocalVariable]()

  def allocateLocal(name: String, valueType: Type): LocalVariable = {
    val index = nextLocalVariableIndex
    nextLocalVariableIndex += valueType.size
    val localVariable = LocalVariable(index, name, valueType)
    localVariables :+= localVariable
    localVariable
  }

  def declareLocalVariables(start: Label, end: Label): Unit = {
    for (localVariable ← localVariables) {
      mv.visitLocalVariable(
        localVariable.name,
        localVariable.valueType.descriptor,
        localVariable.valueType.signature,
        start,
        end,
        localVariable.index
      )
    }
  }

  def getfield(field: Field): Unit = {
    mv.visitFieldInsn(GETFIELD, field.ownerType.internalClassName, field.name, field.valueType.descriptor)
  }

  def getstatic(field: Field): Unit = {
    mv.visitFieldInsn(GETSTATIC, field.ownerType.internalClassName, field.name, field.valueType.descriptor)
  }

  def `this`: LocalVariable = {
    local(0)
  }

  def loadConstant[T](value: T): Unit = {
    if (value == null) {
      mv.visitInsn(ACONST_NULL)
    } else {
      mv.visitLdcInsn(value)
    }
  }

  def loadDefaultValue(valueType: Type): Unit = {
    valueType match {
      case `byte` ⇒
        mv.visitLdcInsn(0.toByte)

      case `char` ⇒
        mv.visitLdcInsn('\u0000')

      case `double` ⇒
        mv.visitLdcInsn(0.0)

      case `float` ⇒
        mv.visitLdcInsn(0.0f)

      case `int` ⇒
        mv.visitLdcInsn(0)

      case `long` ⇒
        mv.visitLdcInsn(0L)

      case `short` ⇒
        mv.visitLdcInsn(0.toShort)

      case `boolean` ⇒
        mv.visitLdcInsn(false)

      case _ ⇒
        mv.visitInsn(ACONST_NULL)
    }
  }

  def local(index: Int): LocalVariable = {
    localVariables(index)
  }

  def local(name: String): LocalVariable = {
    localVariables.find(_.name == name).getOrElse(throw new IllegalArgumentException(s"Unknown local variable: $name"))
  }

  def parameter(index: Int): LocalVariable = {
    ???
  }

  def parameter(name: String): LocalVariable = {
    ???
  }

  def load(localVariable: LocalVariable): Unit = {
    mv.visitVarInsn(localVariable.valueType.opcode(asm.Opcodes.ILOAD), localVariable.index)
  }

  def store(localVariable: LocalVariable): Unit = {
    mv.visitVarInsn(localVariable.valueType.opcode(asm.Opcodes.ISTORE), localVariable.index)
  }

  def getField(field: Field): Unit = {
    ???
  }

  def putfield(field: Field): Unit = {
    mv.visitFieldInsn(asm.Opcodes.PUTFIELD, ownerType.internalClassName, field.name, field.valueType.descriptor)
  }

  def invokevirtual(invocation: Invocation): Unit = {
    mv.visitMethodInsn(INVOKEVIRTUAL, invocation.ownerType.internalClassName, invocation.name, invocation.descriptor, invocation.isInterface)
  }

  def invokeinterface(invocation: Invocation): Unit = {
    mv.visitMethodInsn(INVOKEINTERFACE, invocation.ownerType.internalClassName, invocation.name, invocation.descriptor, invocation.isInterface)
  }

  def invokespecial(invocation: Invocation): Unit = {
    mv.visitMethodInsn(INVOKESPECIAL, invocation.ownerType.internalClassName, invocation.name, invocation.descriptor, invocation.isInterface)
  }

  def goto(destination: Label): Unit = {
    mv.visitJumpInsn(GOTO, destination)
  }

  def label(label: Label): Unit = {
    mv.visitLabel(label)
  }

  def whileLoop(condition: MethodGenerator ⇒ Unit, body: MethodGenerator ⇒ Unit): Unit = {
    val startOfLoop = new Label
    val endOfLoop = new Label
    val startOfCondition = new Label
    val endOfCondition = new Label
    val startOfBody = new Label
    val endOfBody = new Label


    label(startOfLoop)

    {
      label(startOfCondition)
      condition(this) // Causes side-effects
      jumpIfEqualToZero(endOfLoop)
      label(endOfCondition)

      label(startOfBody)
      body(this) // Causes side-effects
      label(endOfBody)

      goto(startOfCondition)
    }

    label(endOfLoop)
  }

  def stringSwitch(keyLocal: LocalVariable, indexLocal: LocalVariable, cases: List[(String, MethodGenerator ⇒ Unit)], defaultCase: MethodGenerator ⇒ Unit): Unit = {
    val keys = cases.map(_._1)
    val keyLabels = keys.map(_ ⇒ new Label)

    val hashCodes = keys.map(_.hashCode).distinct.sorted
    val hashCodeLabels = hashCodes.map(_ ⇒ new Label)


    val startOfHashCodeSwitch = new Label
    val endOfHashCodeSwitch = new Label

    val startOfStringIndexSwitch = new Label
    val endOfStringIndexSwitch = new Label

    label(startOfHashCodeSwitch)

    case class StringCase(key: String, caseGenerator: MethodGenerator ⇒ Unit, index: Int)

    val stringCases: List[StringCase] = for (((key, caseGenerator), index) ← cases.zipWithIndex) yield {
      StringCase(key, caseGenerator, index)
    }

    val `java.lang.String.hashCode()` = `java.lang.String`.invocation("hashCode", `int`, List(), isInterface = false)
    val `java.lang.String.equals(Object)` = `java.lang.String`.invocation("equals", `boolean`, List(`java.lang.Object`), isInterface = false)

    {
      label(startOfHashCodeSwitch)

      loadConstant(-1)
      store(indexLocal)

      load(keyLocal)
      invokevirtual(`java.lang.String.hashCode()`)

      lookupSwitch(
        cases = for (hashCode ← hashCodes) yield hashCode → { (_: MethodGenerator) ⇒
          val filteredStringCases = stringCases.filter(_.key.hashCode == hashCode)

          val hashCodeCaseLabels = filteredStringCases.map(_ ⇒ new Label)
          val nextHashCodeCaseLabels = hashCodeCaseLabels.drop(1) :+ endOfHashCodeSwitch

          for ((stringCase, (caseLabel, nextCaseLabel)) ← filteredStringCases zip (hashCodeCaseLabels zip nextHashCodeCaseLabels)) {
            label(caseLabel)
            load(keyLocal)
            loadConstant(stringCase.key)
            invokevirtual(`java.lang.String.equals(Object)`)
            jumpIfEqualToZero(nextCaseLabel)
            loadConstant(stringCase.index)
            store(indexLocal)
            goto(endOfHashCodeSwitch)
          }
        },
        defaultCase = { _ ⇒
        }
      )

      label(endOfHashCodeSwitch)
    }

    {
      label(startOfStringIndexSwitch)

      load(indexLocal)
      tableSwitch(
        minKey = 0,
        cases = for ((stringCase, keyLabel) ← stringCases zip keyLabels) yield {
          { (_: MethodGenerator) ⇒
            stringCase.caseGenerator(this)
          }
        },
        defaultCase = { _ ⇒
          defaultCase(this)
        }
      )

      label(endOfStringIndexSwitch)
    }
  }

  def lookupSwitch(cases: List[(Int, MethodGenerator ⇒ Unit)], defaultCase: MethodGenerator ⇒ Unit): Unit = {
    val keys = cases.map(_._1)
    val keyLabels = keys.map(_ ⇒ new Label)

    val defaultLabel = new Label

    val startOfSwitch = new Label
    val endOfSwitch = new Label

    mv.visitLookupSwitchInsn(defaultLabel, keys.toArray, keyLabels.toArray)

    for ((keyLabel, generateCase) ← keyLabels zip cases.map(_._2)) {
      label(keyLabel)
      generateCase(this)
      goto(endOfSwitch)
    }

    label(defaultLabel)
    defaultCase(this)

    label(endOfSwitch)
  }

  def tableSwitch(minKey: Int, cases: List[MethodGenerator ⇒ Unit], defaultCase: MethodGenerator ⇒ Unit): Unit = {
    val maxKey = minKey + cases.length - 1

    val caseLabels = cases.map(_ ⇒ new Label)
    val defaultLabel = new Label

    val startOfSwitch = new Label
    val endOfSwitch = new Label

    mv.visitTableSwitchInsn(minKey, maxKey, defaultLabel, caseLabels: _*)
    for ((c, l) ← cases zip caseLabels) {
      label(l)
      c(this)
      goto(endOfSwitch)
    }
    label(defaultLabel)
    defaultCase(this)
    label(endOfSwitch)
  }

  def jumpIfEqualToZero(destination: Label): Unit = {
    mv.visitJumpInsn(IFEQ, destination)
  }

  def pop(): Unit = {
    mv.visitInsn(POP)
  }

  def dup(): Unit = {
    mv.visitInsn(DUP)
  }

  def nop(): Unit = {
    mv.visitInsn(NOP)
  }

  def `return`(valueType: Type): Unit = {
    mv.visitInsn(valueType.opcode(IRETURN))
  }

  def checkcast(t: Type): Unit = {
    mv.visitTypeInsn(CHECKCAST, t.internalClassName)
  }

}
