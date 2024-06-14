package co.blocke

import org.openjdk.jmh.annotations._
import java.util.concurrent.TimeUnit

import ZIOZ.*
import zio.json._
val record = jsData2.fromJson[Record2] match
  case Right(r) => r 
  case Left(_) => null.asInstanceOf[Record2]


trait HandTooledWritingBenchmark {
  private def escapeIfRequire(s: String, i: Int = 0): String = // s
    if (i < s.length) {
      val ch = s.charAt(i)
      if (ch >= ' ' && ch != '\\' && ch != '"') escapeIfRequire(s, i + 1)
      else ???
    } else s

  @Benchmark
  def writeRecordHandTooled = 
    val sb = new java.lang.StringBuilder(1024)
    val person = record.person 
    sb.append("{\"person\":{\"name:\":\"")
    sb.append(escapeIfRequire(person.name))
    sb.append("\",\"age:\":")
    sb.append(person.age)
    sb.append(",\"address:\":{\"street\":")
    val address = person.address 
    sb.append(escapeIfRequire(address.street))
    sb.append("\",\"city\":\"")
    sb.append(escapeIfRequire(address.city))
    sb.append("\",\"state\":\"")
    sb.append(escapeIfRequire(address.state))
    sb.append("\",\"postal_code\":\"")
    sb.append(escapeIfRequire(address.postal_code))
    sb.append("\"},\"email:\":\"")
    sb.append(escapeIfRequire(person.email))
    sb.append("\",\"phone_numbers:\":[")
    person.phone_numbers.foreach {
      var first = true  
      p =>
        if (first) first = false
        else sb.append(',') 
        sb.append('\"')
        sb.append(escapeIfRequire(p))
        sb.append('\"')
    }
    sb.append("],\"is_employed:\":")
    sb.append(record.person.is_employed)
    sb.append("},\"hobbies:\":[")
    record.hobbies.foreach {
      var first = true
      p =>
        if (first) first = false
        else sb.append(',')
        sb.append('\"')
        sb.append(escapeIfRequire(p))
        sb.append('\"')
    }
    sb.append("],\"friends:\":[")
    record.friends.foreach {
      var first = true
      f =>
        if (first) first = false
        else sb.append(',')
        sb.append("""{"name":"""")
        sb.append(escapeIfRequire(f.name))
        sb.append("""","age":""")
        sb.append(f.age)
        sb.append(""","email":"""")
        sb.append(escapeIfRequire(f.email))
        sb.append(""""},""")
    }
    sb.append("],\"pets:\":[")
    record.pets.foreach {
      var first = true
      f =>
        if (first) first = false
        else sb.append(',')
        sb.append("""{"name":"""")
        sb.append(escapeIfRequire(f.name))
        sb.append(""","species":"""")
        sb.append(escapeIfRequire(f.species))
        sb.append("""","age":""")
        sb.append(f.age)
        sb.append('}')
    }
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
