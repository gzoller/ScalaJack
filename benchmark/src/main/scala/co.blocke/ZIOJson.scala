package co.blocke

import zio.json._
import org.openjdk.jmh.annotations._

implicit val decoder1: JsonDecoder[Address] = DeriveJsonDecoder.gen[Address]
implicit val decoder2: JsonDecoder[Pet] = DeriveJsonDecoder.gen[Pet]
implicit val decoder3: JsonDecoder[Friend] = DeriveJsonDecoder.gen[Friend]
implicit val decoder4: JsonDecoder[Person] = DeriveJsonDecoder.gen[Person]
implicit val decoder5: JsonDecoder[Record] = DeriveJsonDecoder.gen[Record]
implicit val encoder1: JsonEncoder[Address] = DeriveJsonEncoder.gen[Address]
implicit val encoder2: JsonEncoder[Pet] = DeriveJsonEncoder.gen[Pet]
implicit val encoder3: JsonEncoder[Friend] = DeriveJsonEncoder.gen[Friend]
implicit val encoder4: JsonEncoder[Person] = DeriveJsonEncoder.gen[Person]
implicit val encoder5: JsonEncoder[Record] = DeriveJsonEncoder.gen[Record]

trait ZIOJsonWritingBenchmark { 
  @Benchmark
  def writeRecordZIOJson = record.toJson
}