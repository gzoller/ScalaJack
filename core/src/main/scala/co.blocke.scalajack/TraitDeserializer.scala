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
    val deserializerClass =
      ScalaCompiler.compileClass(
        s"""import co.blocke.{ scalajack => sj }
           |
           |class ThisTraitDeserializer extends sj.Deserializer[${tt.tpe}] {
           |}
           |
        |scala.reflect.classTag[ThisTraitDeserializer].runtimeClass
      """.stripMargin
      )

    deserializerClass.getConstructor().newInstance().asInstanceOf[TraitDeserializer[T]]
  }

}

class TraitDeserializer[T] extends Deserializer[T] {

  override def deserialize[J](path: Path, json: J)(implicit ops: JsonOps[J]): DeserializationResult[T] = ???

}
