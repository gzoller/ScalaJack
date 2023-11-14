package co.blocke

import org.openjdk.jmh.annotations._

object JawnZ:

    import org.typelevel.jawn.ast.*
   
    trait JawnReadingBenchmark{
        @Benchmark
        def readRecordFabric = JParser.parseFromString(jsData)
        }

    // trait CirceWritingBenchmark { 
    //     @Benchmark
    //     def writeRecordCirce = record.asJson
    //     }
