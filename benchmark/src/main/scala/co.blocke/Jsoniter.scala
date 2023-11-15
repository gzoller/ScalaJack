package co.blocke

import org.openjdk.jmh.annotations._

object JsoniterZ:

    import com.github.plokhotnyuk.jsoniter_scala.core._
    import com.github.plokhotnyuk.jsoniter_scala.macros._
   
    given codec: JsonValueCodec[Record] = JsonCodecMaker.make
    trait JsoniterReadingBenchmark{
        @Benchmark
        def readRecordJsoniter = readFromString[Record](jsData)
        }

    trait JsoniterWritingBenchmark{
        @Benchmark
        def writeRecordJsoniter = writeToString(record)
        }
