package co.blocke.scalajack
package misc

import ScalaJackSyntax.*
import co.blocke.scalajack.json.writing.JsonOutput
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.*

import java.util.concurrent.Executors
import scala.concurrent.duration.*
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Random

object ConcurrencySpec:
  enum Channel:
    case Edi, Api, Ui

  sealed trait Animal derives CanEqual
  case class Fish(species: String, freshwater: Boolean) extends Animal
  case class Bird(species: String, canFly: Boolean) extends Animal

  case class Address(street: String, city: String, state: String, zip: String)
  case class Line(sku: String, qty: Int, price: BigDecimal, tags: List[String])
  case class Payload(
      id: String,
      customer: String,
      address: Address,
      lines: List[Line],
      attributes: Map[String, String],
      animal: Animal,
      channel: Channel,
      notes: Option[String]
  )

  given ScalaJack[Payload] = ScalaJack.sjCodecOf[Payload]

  val payloads: Vector[Payload] =
    (1 to 64).toVector.map { i =>
      Payload(
        id = s"PO-$i",
        customer = s"Customer-$i",
        address = Address(
          street = s"${100 + i} Main St",
          city = if i % 2 == 0 then "Houston" else "Spring",
          state = "TX",
          zip = f"${77000 + i}%05d"
        ),
        lines = List(
          Line(s"SKU-$i-A", i % 7 + 1, BigDecimal("12.34"), List("cold", "fragile", s"batch-$i")),
          Line(s"SKU-$i-B", i % 5 + 2, BigDecimal("56.78"), List("frozen", "bulk", s"zone-${i % 3}"))
        ),
        attributes = Map(
          "po" -> s"03400$i",
          "dest" -> s"03400$i-B",
          "quoted" -> s"""value "$i"""",
          "slashes" -> s"path/$i/end",
          "unicode" -> s"pinata-$i"
        ),
        animal = if i % 2 == 0 then Fish(s"Beta-$i", freshwater = i % 4 == 0) else Bird(s"Hawk-$i", canFly = true),
        channel = if i % 3 == 0 then Channel.Api else if i % 3 == 1 then Channel.Edi else Channel.Ui,
        notes = if i % 4 == 0 then Some(s"Line1\\nLine2-$i") else None
      )
    }

class ConcurrencySpec() extends AnyFunSpec:
  import ConcurrencySpec.*

  private def withExecutionContext[A](parallelism: Int)(f: ExecutionContext => A): A =
    val pool = Executors.newFixedThreadPool(parallelism)
    val ec = ExecutionContext.fromExecutorService(pool)
    try f(ec)
    finally
      pool.shutdown()
      ()

  describe(TestUtil.colorString("-------------------------------\n:      Concurrency Tests      :\n-------------------------------", Console.YELLOW)) {
    it("shared codec should survive heavy concurrent default toJson/fromJson usage") {
      val sharedCodec = summon[ScalaJack[Payload]]
      val rounds = 4000

      withExecutionContext(parallelism = 16) { ec =>
        val work = Future.traverse(0 until rounds) { n =>
          Future {
            val payload = payloads(n % payloads.size)
            val json = sharedCodec.toJson(payload)
            val decoded = sharedCodec.fromJson(json)
            decoded shouldEqual payload

            val jsonViaSyntax = payload.toJson
            val decodedViaSyntax = jsonViaSyntax.fromJson[Payload]
            decodedViaSyntax shouldEqual payload

            val list = List(payload, payloads((n + 1) % payloads.size), payloads((n + 2) % payloads.size))
            val listJson = sharedCodec.toJsonList(list)
            val listDecoded = sharedCodec.fromJsonList(listJson)
            listDecoded shouldEqual list
          }(ec)
        }(implicitly, ec)

        Await.result(work, 60.seconds)
      }
    }

    it("caller-managed JsonOutput reuse should be safe when confined to one thread") {
      val sharedCodec = summon[ScalaJack[Payload]]
      val roundsPerWorker = 1500
      val workers = 8

      withExecutionContext(parallelism = workers) { ec =>
        val work = Future.traverse(0 until workers) { workerId =>
          Future {
            val out = JsonOutput()
            val rng = new Random(workerId.toLong)

            var i = 0
            while i < roundsPerWorker do
              val payload = payloads(rng.nextInt(payloads.size))
              val json = sharedCodec.toJson(payload, out)
              val decoded = sharedCodec.fromJson(json)
              decoded shouldEqual payload

              val list = List(payload, payloads((workerId + i) % payloads.size))
              val listJson = sharedCodec.toJsonList(list, out)
              val listDecoded = sharedCodec.fromJsonList(listJson)
              listDecoded shouldEqual list
              i += 1
          }(ec)
        }(implicitly, ec)

        Await.result(work, 60.seconds)
      }
    }
  }
