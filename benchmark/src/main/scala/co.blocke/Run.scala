package co.blocke

case class Foo() extends ZIOJsonWritingBenchmark

object RunMe extends App:

    val f = Foo()
    println(f.writeRecordZIOJson)