package co.blocke

import org.openjdk.jmh.annotations._

object FabricZ:
    import fabric.*
    import fabric.io.*
    
    trait FabricReadingBenchmark{
        @Benchmark
        def readRecordFabric = JsonParser(jsData, Format.Json)
        }

    // trait CirceWritingBenchmark { 
    //     @Benchmark
    //     def writeRecordCirce = record.asJson
    //     }
