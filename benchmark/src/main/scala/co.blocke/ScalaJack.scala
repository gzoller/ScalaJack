package co.blocke

import org.openjdk.jmh.annotations._

object ScalaJackZ:
    import co.blocke.scalajack.ScalaJack.*
    import co.blocke.scalajack.*
 
    implicit val blah: ScalaJack[co.blocke.Record2] = codecOf[co.blocke.Record2]

    trait ScalaJackReadingBenchmark{
        @Benchmark
        def readRecordScalaJack = ScalaJack[co.blocke.Record2].fromJson(jsData2)
    }

    trait ScalaJackWritingBenchmark { 
        @Benchmark
        def writeRecordScalaJack = ScalaJack[co.blocke.Record2].toJson(record)  
    }

    /* 
    This is the way:

    * Use implicit to define ScalaJack[...] = sj[...]
    * Then use ScalaJack[...].___() to do json function

    implicit val blah: ScalaJack[co.blocke.Record2] = sj[co.blocke.Record2]
    def writeRecordScalaJack = ScalaJack[co.blocke.Record2].toJson(record)  

    TODO: Maybe rewrite sj to be something like buildCodec or something more descriptive.
    */