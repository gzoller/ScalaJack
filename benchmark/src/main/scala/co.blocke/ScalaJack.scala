package co.blocke

import org.openjdk.jmh.annotations._

object ScalaJackZ:
    import co.blocke.scalajack.ScalaJack.*
    import co.blocke.scalajack.*
 
    implicit val blah: ScalaJack[co.blocke.Record2] = sj[co.blocke.Record2]

    trait ScalaJackReadingBenchmark{
        @Benchmark
        def readRecordScalaJack = ScalaJack[co.blocke.Record2].fromJson(jsData2)
    }

    trait ScalaJackWritingBenchmark { 
        @Benchmark
        def writeRecordScalaJack = ScalaJack[co.blocke.Record2].toJson(record)  
        // 344702.052 with escaped strings (apache lib)
        // 2225843.198 with FastStringBuilder
        // 1081490.833 with StringBuilder
    }
