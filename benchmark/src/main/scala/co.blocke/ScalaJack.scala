package co.blocke

import org.openjdk.jmh.annotations._

object ScalaJackZ:
    import co.blocke.scalajack.* 

    import json.*
 
    implicit val blah: sj[Record] = ScalaJack.inspect[Record]

    trait ScalaJackReadingBenchmark{
    @Benchmark
    // def readRecordScalaJack = ScalaJack.read[Record](jsData)
    def readRecordScalaJack = sj[Record].decodeJson(jsData)
   }

    trait ScalaJackWritingBenchmark { 
    @Benchmark
    def writeRecordScalaJack = ScalaJack.write(record)
    }
