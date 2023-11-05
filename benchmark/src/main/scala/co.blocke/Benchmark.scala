package co.blocke

import org.openjdk.jmh.annotations._

import java.util.concurrent.TimeUnit

import co.blocke.scalajack.* 

import io.circe.syntax.*
import io.circe.*
import io.circe.generic.semiauto.*

val record = ScalaJack.read[Record](jsData)

implicit val recordDecoder: Decoder[Record] = deriveDecoder[Record]
implicit val recordEncoder: Encoder[Record] = deriveEncoder[Record]

implicit val personDecoder: Decoder[Person] = deriveDecoder[Person]
implicit val personEncoder: Encoder[Person] = deriveEncoder[Person]

implicit val addressDecoder: Decoder[Address] = deriveDecoder[Address]
implicit val addressEncoder: Encoder[Address] = deriveEncoder[Address]

implicit val friendDecoder: Decoder[Friend] = deriveDecoder[Friend]
implicit val friendEncoder: Encoder[Friend] = deriveEncoder[Friend]

implicit val petDecoder: Decoder[Pet] = deriveDecoder[Pet]
implicit val petEncoder: Encoder[Pet] = deriveEncoder[Pet]



trait CirceReadingBenchmark{
  val circeJson = record.asJson
  def readRecordCirce = circeJson.as[Record]
}

// trait ScalaJackReadingBenchmark{
//   def readRecordScalaJack = ScalaJack.read[Record](jsData)
// }

trait CirceWritingBenchmark { 
  @Benchmark
  def writeRecordCirce = record.asJson
}

trait ScalaJackWritingBenchmark { 
  @Benchmark
  def writeRecordScalaJack = ScalaJack.write(record)
}

trait HandTooledWritingBenchmark { 
  @Benchmark
  def writeRecordHandTooled = 
    val sb = new StringBuilder()
    sb.append("{")
    sb.append("\"person\":{")
    sb.append("\"name:\":\""+record.person.name+"\",")
    sb.append("\"age:\":"+record.person.age+",")
    sb.append("\"address:\":{\"street\":"+record.person.address.street+"\",")
    sb.append("\"city\":\""+record.person.address.city+"\",")
    sb.append("\"state\":\""+record.person.address.state+"\",")
    sb.append("\"postal_code\":\""+record.person.address.postal_code+"\"},")
    sb.append("\"email:\":\""+record.person.email+"\",")
    sb.append("\"phone_numbers:\":[")
    record.person.phone_numbers.map(p => sb.append("\""+p+"\","))
    sb.append("],")
    sb.append("\"is_employed:\":"+record.person.is_employed+"},")
    sb.append("\"hobbies:\":[")
    record.hobbies.map(p => sb.append("\""+p+"\","))
    sb.append("],")
    sb.append("\"friends:\":[")
    record.friends.map(f=>sb.append(s"""{"name":"${f.name},"age":${f.age},"email":"${f.email}"},"""))
    sb.append("],")
    sb.append("\"pets:\":[")
    record.pets.map(f=>sb.append(s"""{"name":"${f.name},"species":"${f.species}"","age":${f.age}},"""))
    sb.append("]}")
    sb.toString
  }

// @State(Scope.Thread)
// @BenchmarkMode(Array(Mode.Throughput))
// @OutputTimeUnit(TimeUnit.SECONDS)
// class ReadingBenchmark
//     extends CirceReadingBenchmark
//     with ScalaJackReadingBenchmark

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class WritingBenchmark
    extends CirceWritingBenchmark
    with ScalaJackWritingBenchmark
    with HandTooledWritingBenchmark
    with ArgonautWritingBenchmark
    with PlayWritingBenchmark
    with ZIOJsonWritingBenchmark
