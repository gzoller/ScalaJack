package co.blocke

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit

import ZIOZ.*
import zio.json._
val record = jsData2.fromJson[Record2] match
  case Right(r) => r 
  case Left(_) => null.asInstanceOf[Record2]


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

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class ReadingBenchmark
    extends ScalaJackZ.ScalaJackReadingBenchmark
    with CirceZ.CirceReadingBenchmark
    with JsoniterZ.JsoniterReadingBenchmark
    with ZIOZ.ZIOJsonReadingBenchmark
    with PlayZ.PlayReadingBenchmark
    with ArgonautZ.ArgonautReadingBenchmark

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class WritingBenchmark
    extends HandTooledWritingBenchmark
    with CirceZ.CirceWritingBenchmark
    with ScalaJackZ.ScalaJackWritingBenchmark
    with JsoniterZ.JsoniterWritingBenchmark
    with ZIOZ.ZIOJsonWritingBenchmark
    with PlayZ.PlayWritingBenchmark
    with ArgonautZ.ArgonautWritingBenchmark
