package co.blocke.scalajack.typeadapter

import java.nio.file.{Files, Paths, StandardOpenOption}

import co.blocke.scalajack.bytecode.BytecodeGenerator
import co.blocke.scalajack.{EmptyReader, _}

abstract class AbstractCaseClassTypeAdapter[T >: Null] extends TypeAdapter[T] {

  override def read(reader: Reader): T = {
    reader.peek match {
      case TokenType.Null =>
        null

      case TokenType.BeginObject =>
        readNonNull(reader)
    }
  }

  override def write(instanceOfCaseClass: T, writer: Writer): Unit = {
    if (instanceOfCaseClass == null) {
      writer.writeNull()
    } else {
      writeNonNull(instanceOfCaseClass, writer)
    }
  }

  def readNonNull(reader: Reader): T

  def writeNonNull(instanceOfCaseClass: T, writer: Writer): Unit

}

object CaseClassTypeAdapterOptimizer {

  def optimize[T >: Null](ta: CaseClassTypeAdapter[T]): TypeAdapter[T] = {
    import co.blocke.scalajack.bytecode.{ClassType, Field, LocalVariable, Type}
    import Type._

    val tpe = ta.tpe

    val caseClassType = typeOf(tpe)

    val childClassName = s"${tpe.typeSymbol.fullName}TypeAdapter"
    val childClassType = ClassType(childClassName, List())

    val `Reader` = typeOf[Reader]
    val `Reader.beginObject()` = `Reader`.invocation("beginObject", `void`, List(), isInterface = true)
    val `Reader.endObject()` = `Reader`.invocation("endObject", `void`, List(), isInterface = true)
    val `Reader.hasMoreMembers()` = `Reader`.invocation("hasMoreMembers", `boolean`, List(), isInterface = true)
    val `Reader.skipValue()` = `Reader`.invocation("skipValue", `void`, List(), isInterface = true)

    val `Writer` = typeOf[Writer]

    val `TypeAdapter` = typeOf[TypeAdapter[_]]()
    val `TypeAdapter.read(Reader)` = `TypeAdapter`.invocation("read", `java.lang.Object`, List(`Reader`), isInterface = true)

    val bytecode = BytecodeGenerator.defineClass(childClassName, typeOf[AbstractCaseClassTypeAdapter[_]](typeOf(tpe))) { c =>
      import c._

      val `emptyReader.field` = defineField("emptyReader", `Reader`)
      val `memberNameTypeAdapter.field` = defineField("memberNameTypeAdapter", typeOf[TypeAdapter[MemberName]])

      case class MemberField(name: String,
                             index: Int,
                             valueType: Type,
                             valueTypeAdapterField: Field)

      val memberFields = ta.members map { member =>
        val valueType = typeOf(member.valueType)
        MemberField(member.name, member.index, valueType, defineField(s"${member.name}TypeAdapter", typeOf[TypeAdapter[_]](valueType)))
      }

      defineConstructorFromFields()

      defineMethod("readNonNull", `java.lang.Object`, "reader" -> `Reader`) { m =>
        import m._

        val `reader.local` = local("reader")
        val `memberNameTypeAdapter.local` = allocateLocal(`memberNameTypeAdapter.field`.name, `memberNameTypeAdapter.field`.valueType)
        val `memberName.local` = allocateLocal("memberName", `java.lang.String`)
        val `memberIndex.local` = allocateLocal("memberIndex", `int`)
        val `membersPresent.local` = allocateLocal("membersPresent", `int`)

        case class MemberLocal(name: String,
                               index: Int,
                               valueType: Type,
                               valueTypeAdapterField: Field,
                               valueLocal: LocalVariable)

        val memberL = memberFields map { memberField =>
          val valueType = memberField.valueType
          val valueLocal = allocateLocal(memberField.name, valueType)
          MemberLocal(memberField.name, memberField.index, valueType, memberField.valueTypeAdapterField, valueLocal)
        }

        for (member <- memberL) {
          loadDefaultValue(member.valueType)
          store(member.valueLocal)
        }

        load(`this`)
        getfield(`memberNameTypeAdapter.field`)
        store(`memberNameTypeAdapter.local`)

        load(`reader.local`)
        invokeinterface(`Reader.beginObject()`)

        whileLoop(
          condition = { _ =>
            load(`reader.local`)
            invokeinterface(`Reader.hasMoreMembers()`)
          },
          body = { _ =>
            load(`memberNameTypeAdapter.local`)
            load(`reader.local`)
            invokeinterface(`TypeAdapter.read(Reader)`)
            cast(from = `TypeAdapter.read(Reader)`.returnType, to = `memberName.local`.valueType)
            store(`memberName.local`)

            load(`reader.local`)
            invokeinterface(`Reader.skipValue()`)

            //                load(`memberName.local`)
            //                stringSwitch(
            //                  keyLocal = `memberName.local`,
            //                  indexLocal = `memberIndex.local`,
            //                  cases =
            //                    memberL map { member =>
            //                      member.name -> { (_: MethodGenerator) =>
            //                        load(`this`)
            //                        getfield(member.valueTypeAdapterField)
            //                        load(`reader.local`)
            //                        invokeinterface(`TypeAdapter.read(Reader)`)
            //                        cast(from = `TypeAdapter.read(Reader)`.returnType, to = member.valueLocal.valueType)
            //                        store(member.valueLocal)

            //                        load(`membersPresent.local`)
            //                        loadConstant(1 << member.index)
            //                        ior()
            //                        store(`membersPresent.local`)
            //                      }
            //                    },
            //                  defaultCase = { _ =>
            //                  }
            //                )

            //                load(`this`)
            //                load(`reader.local`)
            //                invokeinterface(`Reader.skipValue()`)
          }
        )

        load(`reader.local`)
        invokeinterface(`Reader.endObject()`)

        `new`(caseClassType)
        dup()
        for (member <- memberL) {
          load(member.valueLocal)
        }
        invokespecial(caseClassType.invocation("<init>", `void`, memberL.map(_.valueType), isInterface = false))

        `return`(`java.lang.Object`)
      }

      defineMethod("writeNonNull", `void`, "instanceOfCaseClass" -> caseClassType, "writer" -> `Writer`) { m =>
        import m._

        `return`()
      }
    }

    Files.write(Paths.get("/Users/tue880/Foo.class"), bytecode, StandardOpenOption.CREATE)

    object DynamicClassLoader extends ClassLoader {

      def defineTypeAdapterClass() = {
        defineClass(childClassType.className, bytecode, 0, bytecode.length)
      }

    }

    val c = DynamicClassLoader.defineTypeAdapterClass()

    c.getConstructors()(0).newInstance(List(EmptyReader, ta.memberNameTypeAdapter) ::: ta.members.map(_.valueTypeAdapter): _*).asInstanceOf[TypeAdapter[T]]
  }

}
