package co.blocke

import org.openjdk.jmh.annotations._

object ScalaJackZ:
    import co.blocke.scalajack.ScalaJack.*
    import co.blocke.scalajack.*
 
    implicit val blah: ScalaJack[co.blocke.Record2] = sj[co.blocke.Record2]

    trait ScalaJackReadingBenchmark{
        @Benchmark
        // def readRecordScalaJack = sj[Record].fromJson(jsData) // 500K
        def readRecordScalaJack = ScalaJack[co.blocke.Record2].fromJson(jsData2) // 515K :-(
    }

    trait ScalaJackWritingBenchmark { 
        @Benchmark
        // def writeRecordScalaJack = sj[Record].toJson(record)   // 677K score
        def writeRecordScalaJack = ScalaJack[co.blocke.Record2].toJson(record)  // 1.7M score <- faster
    }
