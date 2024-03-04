package co.blocke

import org.openjdk.jmh.annotations._

object JsoniterZ:

    import com.github.plokhotnyuk.jsoniter_scala.core._
    import com.github.plokhotnyuk.jsoniter_scala.macros._
   
    given codec: JsonValueCodec[Record2] = JsonCodecMaker.make
    trait JsoniterReadingBenchmark{
        @Benchmark
        def readRecordJsoniter = readFromString[Record2](jsData2)
        }

    trait JsoniterWritingBenchmark{
        @Benchmark
        def writeRecordJsoniter = writeToString(record)
        }
