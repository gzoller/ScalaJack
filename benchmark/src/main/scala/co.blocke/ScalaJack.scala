package co.blocke

import org.openjdk.jmh.annotations._

object ScalaJackZ:
    import co.blocke.scalajack.ScalaJack.*
    import co.blocke.scalajack.*
 
    given sj: ScalaJack[co.blocke.Record2] = sjCodecOf[co.blocke.Record2]

    trait ScalaJackReadingBenchmark{
        @Benchmark
        def readRecordScalaJack = sj.fromJson(jsData2)
    }

    trait ScalaJackWritingBenchmark { 
        @Benchmark
        def writeRecordScalaJack = sj.toJson(record)  
    }

    /* 
    This is the way:

    * Use implicit to define ScalaJack[...] = sj[...]
    * Then use ScalaJack[...].___() to do json function

    implicit val blah: ScalaJack[co.blocke.Record2] = sj[co.blocke.Record2]
    def writeRecordScalaJack = ScalaJack[co.blocke.Record2].toJson(record)  
    */