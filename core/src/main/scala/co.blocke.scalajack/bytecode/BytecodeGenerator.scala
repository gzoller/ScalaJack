package co.blocke.scalajack.bytecode

import java.nio.file.{Files, Paths}

import co.blocke.scalajack.{MemberName, Reader, TypeAdapter, Writer}

object Thing2 extends App {

  import Type._

  val `Reader.type` = typeOf[Reader]

  val `java.io.PrintStream` = typeOf[java.io.PrintStream]
  val `java.io.PrintStream.println(String)` = `java.io.PrintStream`.invocation("println", `void`, List(`java.lang.String`), isInterface = false)

  val `java.lang.System` = typeOf[java.lang.System]
  val `java.lang.System.out` = Field(`java.lang.System`, "out", `java.io.PrintStream`)

  val bytecode = BytecodeGenerator.defineClass("foo.Bar", `java.lang.Object`) { c ⇒
    import c._

    val `memberNameTypeAdapter.field` = defineField("memberNameTypeAdapter", typeOf[TypeAdapter[MemberName]])

    val caseClassType = ClassType("my.CaseClass", List())

    case class Member(name: String, valueType: Type, accessor: Invocation)

    val members = List(
      Member("id", `long`, caseClassType.invocation("id", `long`, List(), isInterface = false)),
      Member("name", `java.lang.String`, caseClassType.invocation("name", `java.lang.String`, List(), isInterface = false)),
      Member("children", typeOf[List[String]], caseClassType.invocation("children", typeOf[List[String]], List(), isInterface = false))
    )


    val memberTypeAdapterFields = for (member ← members) yield
      defineField(s"${member.name}TypeAdapter", typeOf[TypeAdapter[_]](member.valueType))


    //    val `java.lang.Object.<init>()` = `java.lang.Object`.invocation("<init>", `void`, List(), isInterface = false)
//
//    defineConstructor(`memberNameTypeAdapter.field`.name → `memberNameTypeAdapter.field`.valueType) { c ⇒
//      import c._
//
//      load(`this`)
//      invokespecial(`java.lang.Object.<init>()`)
//    }

    defineConstructorFromFields()

    defineMethod("read", `java.lang.Object`, "reader" → `Reader.type`) { m ⇒
      import m._

      val `reader.local` = m.local("reader")
      val `memberNameTypeAdapter.local` = allocateLocal(`memberNameTypeAdapter.field`.name, `memberNameTypeAdapter.field`.valueType)
      val `memberName.local` = m.allocateLocal("memberName", `java.lang.String`)
      val `memberIndex.local` = m.allocateLocal("memberIndex", `int`)

      val `Reader.type` = typeOf[Reader]
      val `Reader.hasMoreMembers()` = `Reader.type`.invocation("hasMoreMembers", `boolean`, List(), isInterface = true)
      val `Reader.beginObject()` = `Reader.type`.invocation("beginObject", `void`, List(), isInterface = true)
      val `Reader.endObject()` = `Reader.type`.invocation("endObject", `void`, List(), isInterface = true)

      val `TypeAdapter.type` = typeOf[TypeAdapter[_]]()
      val `TypeAdapter.read(Reader)` = `TypeAdapter.type`.invocation("read", `java.lang.Object`, List(`Reader.type`), isInterface = true)

      load(`this`)
      getfield(`memberNameTypeAdapter.field`)
      store(`memberNameTypeAdapter.local`)

      load(`reader.local`)
      invokeinterface(`Reader.beginObject()`)

      whileLoop(
        condition = { _ ⇒
          load(`reader.local`)
          invokeinterface(`Reader.hasMoreMembers()`)
        },
        body = { _ ⇒
          load(`memberNameTypeAdapter.local`)
          load(`reader.local`)
          invokeinterface(`TypeAdapter.read(Reader)`)
          checkcast(`java.lang.String`)
          store(`memberName.local`)

          stringSwitch(
            keyLocal = `memberName.local`,
            indexLocal = `memberIndex.local`,
            cases = for ((member, memberTypeAdapterField) ← members zip memberTypeAdapterFields) yield {
              member.name → { (_: MethodGenerator) ⇒
                load(`this`)
                getfield(memberTypeAdapterField)
                load(`reader.local`)
                invokeinterface(`TypeAdapter.read(Reader)`)
                pop()
              }
            },
            defaultCase = { _ ⇒

            }
          )
        }
      )

      load(`reader.local`)
      invokeinterface(`Reader.endObject()`)

      loadConstant(null)
      `return`(`java.lang.Object`)
    }
  }

  Files.write(Paths.get("/Users/tue880/Foo.class"), bytecode)

  println(typeOf[java.util.List[java.lang.String]].signature)

}

object Thing {

  val classGenerator = new ClassGenerator(null, null)

  import classGenerator._

  import Type._

  val `foo.field` = defineField("foo", `java.lang.String`)

  val `CC` = typeOf[String]
  val `Reader.type` = typeOf[Reader]
  val `Writer.type` = typeOf[Writer]

  val `Reader.beginObject()` = `Reader.type`.invocation("beginObject", `void`, List(), isInterface = true)
  val `Reader.endObject()` = `Reader.type`.invocation("beginObject", `void`, List(), isInterface = true)
  val `Reader.hasMoreMembers()` = `Reader.type`.invocation("hasMoreMembers", `boolean`, List(), isInterface = true)

  case class Member(name: String, valueType: Type, field: Field, local: LocalVariable)

  val members = List[Member]()

  defineMethod("read", `java.lang.Object`, "reader" → `Reader.type`) { m ⇒
    import m._

    val `reader.local` = parameter("reader")

    for (member ← members) {
      loadDefaultValue(member.valueType)
      store(member.local)
    }

    whileLoop(
      condition = { _ ⇒
        load(`reader.local`)
        invokeinterface(`Reader.hasMoreMembers()`)
      },
      body = { _ ⇒
        stringSwitch(
          keyLocal = null,
          indexLocal = null,
          cases = List(
            "foo" → { _ ⇒
              getField(`foo.field`)
            },
            "bar" → { _ ⇒

            }
          ),
          defaultCase = { _ ⇒

          }
        )
        loadConstant("DONE")
        pop()
      }
    )

    for (member ← members) {

    }

    loadConstant(null)
    `return`(`java.lang.Object`)
  }

  defineMethod("write", `java.lang.Object`, "instanceOfCaseClass" → `CC`, "writer" → `Writer.type`) { m ⇒
    import m._

    local(1)
    val `instanceOfCaseClass` = parameter(0)
    val `writer` = parameter(1)

    load(`this`)
    store(`instanceOfCaseClass`)

    getField(`foo.field`)
    putfield(`foo.field`)

//    invokevirtual()

  }

}

object BytecodeGenerator {

  def defineClass(name: String, superType: Type)(body: ClassGenerator ⇒ Unit): Array[Byte] = {
    import org.objectweb.asm
    import asm.ClassWriter

    val classType = ClassType(name, List())

    val classWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS)

    classWriter.visit(asm.Opcodes.V1_8, asm.Opcodes.ACC_PUBLIC, classType.internalClassName, null, superType.internalClassName, null)

    val classGenerator = new ClassGenerator(classType, classWriter)
    body(classGenerator)

    classWriter.visitEnd()

    classWriter.toByteArray
  }

}
