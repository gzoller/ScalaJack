package co.blocke.scalajack

import java.util.concurrent.{ConcurrentHashMap, TimeUnit}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object TraitDeserializer {

  private val cache = new ConcurrentHashMap[Type, Future[TraitDeserializer[_]]]()

  def apply[T]()(implicit tt: TypeTag[T]): TraitDeserializer[T] = {

    val deserializerFuture: Future[TraitDeserializer[T]] = cache.computeIfAbsent(tt.tpe, new java.util.function.Function[Type, Future[TraitDeserializer[_]]] {
      override def apply(traitType: Type): Future[TraitDeserializer[_]] =
        Future(generateDeserializer[T]())
    }).asInstanceOf[Future[TraitDeserializer[T]]]

    val deserializer = Await.result(deserializerFuture, FiniteDuration(30, TimeUnit.SECONDS))

    deserializer
  }

  def generateDeserializer[T]()(implicit tt: TypeTag[T]): TraitDeserializer[T] = {

    val reflectMembers = tt.tpe.typeSymbol.asClass.typeSignature.members.filter(_.isAbstract).toList

    class FieldMember(val name: String, val valueType: Type)

    val members: List[FieldMember] = reflectMembers map { reflectMember =>
      new FieldMember(name = reflectMember.name.decodedName.toString, valueType = reflectMember.asMethod.returnType)
    }

    val deserializerClass =
      ScalaCompiler.compileClass(
        s"""
           |class Deferred[J](path: co.blocke.scalajack.Path, json: J)(implicit ops: co.blocke.scalajack.JsonOps[J]) extends ${tt.tpe} {
           |
           |${members.map(member => s"""  override lazy val ${member.name}: ${member.valueType} = ???""").mkString("\n\n")}
           |
           |}
           |
           |class ThisTraitDeserializer(implicit tt: co.blocke.scalajack.TypeTag[${tt.tpe}]) extends co.blocke.scalajack.TraitDeserializer[${tt.tpe}] {
           |
           |  import co.blocke.{ scalajack => sj }
           |
           |  override def deserialize[J](path: sj.Path, json: J)(implicit ops: sj.JsonOps[J]): sj.DeserializationResult[${tt.tpe}] = {
           |    sj.DeserializationSuccess(sj.TypeTagged(new Deferred[J](path, json), tt.tpe))
           |  }
           |
           |}
           |
        |scala.reflect.classTag[ThisTraitDeserializer].runtimeClass
      """.stripMargin
      )

    deserializerClass.getConstructor().newInstance().asInstanceOf[TraitDeserializer[T]]
  }

}

trait TraitDeserializer[T] extends Deserializer[T] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[T] = ???

}
