package co.blocke

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit

import ZIOZ.*
import zio.json._
val record = jsData.fromJson[Record] match
  case Right(r) => r 
  case Left(_) => null.asInstanceOf[Record]


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
    // extends CirceZ.CirceReadingBenchmark
    extends ScalaJackZ.ScalaJackReadingBenchmark
    // with JsoniterZ.JsoniterReadingBenchmark
    // with ZIOZ.ZIOJsonReadingBenchmark
    // with PlayZ.PlayReadingBenchmark
    // with ArgonautZ.ArgonautReadingBenchmark

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class WritingBenchmark
    // extends HandTooledWritingBenchmark
    // extends CirceZ.CirceWritingBenchmark
    extends ScalaJackZ.ScalaJackWritingBenchmark
    // extends JsoniterZ.JsoniterWritingBenchmark
    // with ZIOZ.ZIOJsonWritingBenchmark
    // with PlayZ.PlayWritingBenchmark
    // with ArgonautZ.ArgonautWritingBenchmark

// "Old-New" ScalaJack
// [info] Benchmark                              Mode  Cnt        Score    Error  Units
// [info] ReadingBenchmark.readRecordScalaJack  thrpt   20    30113.982 ± 97.701  ops/s
// [info] New-New ScalaJack                     thrpt   20    50908.982 ± 97.701  ops/s

//        ScalaJack w/ZIO-based parser                       635977.008
//        ZIO (Fast!!)                                       568123.000 <-- How do they do this?!  More than 2x faster than everyone else!
//        Circe                                              279231.646
//        Play                                               209756.408

//        Jawn (parse only + AST)                            336384.617
//        ScalaJack JsonParser3 (parse only + AST)           279456.523
//        Fabric (new!) (parse only + AST)                   270706.567
