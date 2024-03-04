package co.blocke

import org.openjdk.jmh.annotations._

object ZIOZ:
  import zio.json._

  implicit val decoder1: JsonDecoder[Address2] = DeriveJsonDecoder.gen[Address2]
  implicit val decoder2: JsonDecoder[Pet2] = DeriveJsonDecoder.gen[Pet2]
  implicit val decoder3: JsonDecoder[Friend2] = DeriveJsonDecoder.gen[Friend2]
  implicit val decoder4: JsonDecoder[Person2] = DeriveJsonDecoder.gen[Person2]
  implicit val decoder5: JsonDecoder[Record2] = DeriveJsonDecoder.gen[Record2]
  implicit val encoder1: JsonEncoder[Address2] = DeriveJsonEncoder.gen[Address2]
  implicit val encoder2: JsonEncoder[Pet2] = DeriveJsonEncoder.gen[Pet2]
  implicit val encoder3: JsonEncoder[Friend2] = DeriveJsonEncoder.gen[Friend2]
  implicit val encoder4: JsonEncoder[Person2] = DeriveJsonEncoder.gen[Person2]
  implicit val encoder5: JsonEncoder[Record2] = DeriveJsonEncoder.gen[Record2]

  trait ZIOJsonWritingBenchmark { 
    @Benchmark
    def writeRecordZIOJson = record.toJson
  }

  trait ZIOJsonReadingBenchmark { 
    @Benchmark
    def readRecordZIOJson = jsData2.fromJson[Record2]
  }