package co.blocke.scalajack

import java.util.concurrent.{ ConcurrentHashMap, TimeUnit }

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }

object TraitDeserializer {

  private val cache = new ConcurrentHashMap[Type, Future[TraitDeserializer[_]]]()

  def apply[T]()(implicit tt: TypeTag[T], context: Context): TraitDeserializer[T] = {

    val deserializerFuture: Future[TraitDeserializer[T]] = cache.computeIfAbsent(tt.tpe, new java.util.function.Function[Type, Future[TraitDeserializer[_]]] {
      override def apply(traitType: Type): Future[TraitDeserializer[_]] =
        Future(generateDeserializer[T]())
    }).asInstanceOf[Future[TraitDeserializer[T]]]

    val deserializer = Await.result(deserializerFuture, FiniteDuration(30, TimeUnit.SECONDS))

    deserializer
  }

  private def repeatUntilConverged[T](initial: T)(f: T => T): T = {
    var current = initial

    var converged = false
    while (!converged) {
      val previous = current
      val next = f(previous)

      if (previous == next) {
        converged = true
      }

      current = next
    }

    current
  }

  def generateDeserializer[T]()(implicit tt: TypeTag[T], context: Context): TraitDeserializer[T] = {
    val classSymbol: ClassSymbol = tt.tpe.typeSymbol.asClass
    val memberSymbols = classSymbol.typeSignature.members.filter(_.isAbstract).toList

    class FieldMember(val name: String, val valueType: Type, val memberSymbol: Symbol) {

      def isPathDependentType: Boolean =
        valueType match {
          case TypeRef(SingleType(ThisType(`classSymbol`), _), _, _) =>
            true
          case _ =>
            false
        }

      def valueTypeAsScala: String =
        valueType match {
          case TypeRef(SingleType(ThisType(`classSymbol`), fieldSymbol), typeMemberOfFieldSymbol, _) =>
            s"${fieldSymbol.name.decodedName}.${typeMemberOfFieldSymbol.name.decodedName}"

          case _ =>
            valueType.toString
        }

    }

    var memberSymbolDependencies = Set.empty[(Symbol, Symbol)]

    var birds: Map[Symbol, Type] = Map.empty

    val members: List[FieldMember] = memberSymbols map { memberSymbol =>
      val fieldName = memberSymbol.name.decodedName.toString
      val fieldValueType = memberSymbol.asMethod.returnType
      var referencedFieldSymbols = Set.empty[Symbol]
      //      fieldValueType.foreach(allTypes += _)

      var maybeDonkey: Option[Symbol] = None

      fieldValueType foreach {
        case t @ TypeRef(donkey @ SingleType(ThisType(`classSymbol`), referencedMemberSymbol), _, _) =>
          //          maybeDonkey = Some(referencedMemberSymbol)
          maybeDonkey = Some(donkey.termSymbol)
          birds += referencedMemberSymbol -> donkey
          referencedFieldSymbols += referencedMemberSymbol
          if (memberSymbols.contains(referencedMemberSymbol)) {
            memberSymbolDependencies += (memberSymbol -> referencedMemberSymbol)
          }
        case t =>
          println(t)
      }

      for (donkey <- maybeDonkey) {
        val from = List(donkey)
        val to = List(typeOf[String])
        val mule = fieldValueType.substituteTypes(from, to)

        //        new scala.reflect.api.Trees.Transformer

        println(mule)
      }

      println(referencedFieldSymbols)
      new FieldMember(name         = fieldName, valueType = fieldValueType, memberSymbol = memberSymbol)
    }

    println(memberSymbolDependencies)

    val symbolSequences = repeatUntilConverged[Seq[Seq[Symbol]]](Seq.empty) { symbolSequencesSoFar: Seq[Seq[Symbol]] =>
      val memberSymbolsBefore = symbolSequencesSoFar.flatten.toSet

      var newMemberSymbols: Seq[Symbol] = Seq.empty

      for (memberSymbol <- memberSymbols.filterNot(memberSymbolsBefore.contains)) {
        val dependentSymbols: Set[Symbol] = memberSymbolDependencies.filter(_._1 == memberSymbol).map(_._2)
        if (dependentSymbols.isEmpty || dependentSymbols.subsetOf(memberSymbolsBefore)) {
          newMemberSymbols :+= memberSymbol
        }
      }

      if (newMemberSymbols.nonEmpty) {
        symbolSequencesSoFar :+ newMemberSymbols
      } else {
        symbolSequencesSoFar
      }
    }

    println(symbolSequences)

    object dependencyGraph {

      def dependencies(memberSymbol: Symbol): Set[Symbol] = memberSymbolDependencies.collect({ case (`memberSymbol`, dependency) => dependency })

      def dependents(memberSymbol: Symbol): Set[Symbol] = memberSymbolDependencies.collect({ case (dependent, `memberSymbol`) => dependent })

    }

    def deserializationFieldDeclaration(member: FieldMember): String = {
      val builder = new StringBuilder

      val dependentMemberSymbols: Set[Symbol] = memberSymbolDependencies.filter(_._1 == member.memberSymbol).map(_._2)

      builder.append(s"""  private lazy val ${member.name}DeserializationResult: sj.DeserializationResult[${member.valueTypeAsScala}] = """)
      if (dependentMemberSymbols.isEmpty) {
        builder.append(s"""deserializeField("${member.name}", fieldSet.${member.name}Deserializer)""")
      } else {
        val dependentMembers = members.filter(m => dependentMemberSymbols.contains(m.memberSymbol))
        builder.append("{\n")
        builder.append(s"    if (${dependentMembers.map(dependentMember => s"${dependentMember.name}DeserializationResult.isFailure").mkString(" || ")}) {\n")
        builder.append(s"      sj.DeserializationFailure(${dependentMembers.map(dependentMember => s"""${dependentMember.name}DeserializationResult.errors""").mkString(" ++ ")})\n")
        builder.append(s"    } else {\n")
        //        for (dependentMember <- dependentMembers) {
        //          builder.append(s"      val sj.DeserializationSuccess(${dependentMember.name}TypeTagged) = ${dependentMember.name}DeserializationResult\n")
        //        }
        //        builder.append(s"""      val childContext = context.addTypeAdapterFactories(${dependentMembers.map(dm => s"""sj.TypeAdapterFactory.=:=[${dm.valueTypeAsScala}](context.typeAdapterOf[${dm.valueTypeAsScala}](sj.TypeTags.of[${dm.valueTypeAsScala}](${dm.name}TypeTagged.tpe)))""").mkString(", ")})\n""")

        for (dm <- dependentMembers) {
          builder.append(s"      val sj.DeserializationSuccess(${dm.name}TypeTagged) = ${dm.name}DeserializationResult\n")
          builder.append(s"      val ${dm.name}RefDeserializer: sj.Deserializer[${dm.name}.type] = sj.Deserializer.constant(${dm.name}TypeTagged.asInstanceOf[sj.TypeTagged[${dm.name}.type]])\n")
          builder.append(s"      val ${dm.name}RefTypeTag: sj.TypeTag[${dm.name}.type] = sj.TypeTags.of[${dm.name}.type](fieldSet.${dm.name}RefType)\n")
          builder.append(s"      val ${dm.name}RefTypeAdapterFactory: sj.TypeAdapterFactory = sj.TypeAdapterFactory.=:=[${dm.name}.type](${dm.name}RefDeserializer)(sj.TypeTags.of[${dm.name}.type](fieldSet.${dm.name}RefType))\n")
          builder.append("\n")
        }

        builder.append(s"      val ${member.name}TypeFinal: sj.Type = fieldSet.${member.name}Type map {\n")
        builder.append(dependentMembers.map(dm => s"""        case fieldSet.${dm.name}RefType => ${dm.name}TypeTagged.tpe""").mkString("\n"))
        builder.append("\n")
        builder.append("        case t => t\n")
        builder.append(s"      }\n")
        builder.append("\n")

        builder.append(s"""      println("final type: " + ${member.name}TypeFinal)""" + "\n")

        builder.append(s"""      val childContext = baseContext.addTypeAdapterFactories(${dependentMembers.map(dm => s"${dm.name}RefTypeAdapterFactory").mkString(", ")})\n""")
        builder.append(s"      val ${member.name}Deserializer: sj.Deserializer[${member.valueTypeAsScala}] = childContext.deserializer(${member.name}TypeFinal).asInstanceOf[sj.Deserializer[${member.valueTypeAsScala}]]\n")
        builder.append(s"""      deserializeField("${member.name}", ${member.name}Deserializer)""" + "\n")
        builder.append(s"    }\n")
        builder.append("  }")
      }

      builder.result()
    }

    val fieldSetParameterDeclarations: Seq[(String, String, Any)] =
      members flatMap { member =>
        var declarations = Seq.empty[(String, String, Any)]

        declarations :+= ((s"""${member.name}Type""", s"""co.blocke.scalajack.Type""", member.valueType))

        if (dependencyGraph.dependencies(member.memberSymbol).isEmpty) {
          declarations :+= ((s"""${member.name}Deserializer""", s"""co.blocke.scalajack.Deserializer[${member.valueTypeAsScala}]""", context.deserializer(member.valueType)))
        }

        if (dependencyGraph.dependents(member.memberSymbol).nonEmpty) {
          declarations :+= ((s"""${member.name}RefType""", s"""co.blocke.scalajack.Type""", birds(member.memberSymbol)))
        }

        declarations
      }

    val deserializerClass =
      ScalaCompiler.compileClass(
        s"""
           |class ThisTraitFieldSet(
           |${fieldSetParameterDeclarations.map({ case (name, valueType, _) => s"""  val $name: $valueType""" }).mkString(",\n")}
           |)
           |
           |class Deferred[J](override val thisPath: co.blocke.scalajack.Path, override val thisJson: J, fieldSet: ThisTraitFieldSet)(implicit override val thisJsonOps: co.blocke.scalajack.JsonOps[J], baseContext: co.blocke.scalajack.Context) extends ${tt.tpe} with co.blocke.scalajack.DeserializedTrait[J] {
           |
           |  import co.blocke.{ scalajack => sj }
           |
           |${members.map(deserializationFieldDeclaration).mkString("\n\n")}
           |
           |${members.map(member => s"""  override lazy val ${member.name}: ${member.valueTypeAsScala} = ${member.name}DeserializationResult.get.get""").mkString("\n\n")}
           |
           |}
           |
           |class ThisTraitDeserializer(${fieldSetParameterDeclarations.map({ case (name, valueType, _) => s"""$name: $valueType""" }).mkString(", ")})(implicit tt: co.blocke.scalajack.TypeTag[${tt.tpe}], baseContext: co.blocke.scalajack.Context) extends co.blocke.scalajack.TraitDeserializer[${tt.tpe}] {
           |
           |  import co.blocke.{ scalajack => sj }
           |
           |  private val fieldSet = new ThisTraitFieldSet(
           |${fieldSetParameterDeclarations.map({ case (name, _, _) => s"""    $name = $name""" }).mkString(",\n")}
           |  )
           |
           |  override def deserialize[J](path: sj.Path, json: J)(implicit ops: sj.JsonOps[J]): sj.DeserializationResult[${tt.tpe}] = {
           |    sj.DeserializationSuccess(sj.TypeTagged(new Deferred[J](path, json, fieldSet), tt.tpe))
           |  }
           |
           |}
           |
           |scala.reflect.classTag[ThisTraitDeserializer].runtimeClass
      """.stripMargin)

    val deserializers = members.filterNot(_.isPathDependentType).map(_.valueType).map(context.deserializer)

    val deserializer = deserializerClass.getConstructors()(0).newInstance(fieldSetParameterDeclarations.map(_._3.asInstanceOf[java.lang.Object]) ++ List(tt, context): _*).asInstanceOf[TraitDeserializer[T]]
    deserializer
  }

}

trait TraitDeserializer[T] extends Deserializer[T] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[T] = ???

}

trait DeserializedTrait[J] {

  val thisPath: Path

  val thisJson: J

  implicit val thisJsonOps: JsonOps[J]

  def deserializeField[V](name: String, valueDeserializer: Deserializer[V]): DeserializationResult[V] =
    thisJson match {
      case JsonObject(x) =>
        val fields = x.asInstanceOf[thisJsonOps.ObjectFields]

        val deserializationResult: DeserializationResult[V] =
          thisJsonOps.getObjectField(fields, name) match {
            case Some(valueJson) =>
              valueDeserializer.deserialize(thisPath \ name, valueJson)

            case None =>
              valueDeserializer.deserializeFromNothing(thisPath \ name)
          }

        deserializationResult
    }

}