package co.blocke

import org.openjdk.jmh.annotations._

object ScalaJackZ:
    import co.blocke.scalajack.ScalaJack.*
    import co.blocke.scalajack.*
 
    implicit val blah: ScalaJack[Record] = sj[Record]

    trait ScalaJackReadingBenchmark{
    @Benchmark
    // def readRecordScalaJack = sj[Record].fromJson(jsData) // 500K
    def readRecordScalaJack = ScalaJack[Record].fromJson(jsData) // 515K :-(
   }

    trait ScalaJackWritingBenchmark { 
    @Benchmark
    // def writeRecordScalaJack = sj[Record].toJson(record)   // 677K score
    def writeRecordScalaJack = ScalaJack[Record].toJson(record)  // 1.7M score <- faster
    }
