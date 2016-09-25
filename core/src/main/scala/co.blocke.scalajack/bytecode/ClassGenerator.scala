package co.blocke.scalajack.bytecode

import org.objectweb.asm

class ClassGenerator(classType: Type, cv: asm.ClassVisitor) {

  import Type._

  var fields = List[Field]()

  val `java.lang.Object.<init>()` = `java.lang.Object`.invocation("<init>", `void`, List(), isInterface = false)

  def defineField(name: String, valueType: Type): Field = {
    cv.visitField(asm.Opcodes.ACC_PUBLIC, name, valueType.descriptor, valueType.signature, null)
    val field = Field(classType, name, valueType)
    fields :+= field
    field
  }

  def defineConstructorFromFields(): Method = {
    defineConstructor(fields.map(field ⇒ field.name → field.valueType): _*) { c ⇒
      import c._

      load(`this`)
      invokespecial(`java.lang.Object.<init>()`)

      for (field ← fields) {
        load(`this`)
        load(local(field.name))
        putfield(field)
      }
    }
  }

  def defineConstructor(parameters: (String, Type)*)(body: MethodGenerator ⇒ Unit): Method = {
    defineMethod("<init>", `void`, parameters: _*)(body)
  }

  def defineMethod(name: String, returnType: Type, parameters: (String, Type)*)(body: MethodGenerator ⇒ Unit): Method = {
    val method = Method(classType, name, returnType, parameters.map(_._2).toList)
    val mv = cv.visitMethod(asm.Opcodes.ACC_PUBLIC, name, method.descriptor, method.signature, null)

    val methodGenerator = new MethodGenerator(classType, mv)

    methodGenerator.allocateLocal("this", classType)
    for ((name, valueType) ← parameters) {
      methodGenerator.allocateLocal(name, valueType)
    }

    mv.visitCode()

    val startOfBody = new asm.Label
    body(methodGenerator)
    val endOfBody = new asm.Label

    methodGenerator.declareLocalVariables(startOfBody, endOfBody)

    method
  }

}
