package co.blocke

import org.openjdk.jmh.annotations._

object CirceZ:
    import io.circe.syntax.*
    import io.circe.*
    import io.circe.generic.semiauto.*
    import io.circe.parser.*
    
    implicit val recordDecoder: Decoder[Record] = deriveDecoder[Record]
    implicit val recordEncoder: Encoder[Record] = deriveEncoder[Record]

    implicit val personDecoder: Decoder[Person] = deriveDecoder[Person]
    implicit val personEncoder: Encoder[Person] = deriveEncoder[Person]

    implicit val addressDecoder: Decoder[Address] = deriveDecoder[Address]
    implicit val addressEncoder: Encoder[Address] = deriveEncoder[Address]

    implicit val friendDecoder: Decoder[Friend] = deriveDecoder[Friend]
    implicit val friendEncoder: Encoder[Friend] = deriveEncoder[Friend]

    implicit val petDecoder: Decoder[Pet] = deriveDecoder[Pet]
    implicit val petEncoder: Encoder[Pet] = deriveEncoder[Pet]

    trait CirceReadingBenchmark{
        @Benchmark
        def readRecordCirce = parse(jsData).flatMap(_.as[Record])
        }

    trait CirceWritingBenchmark { 
        @Benchmark
        def writeRecordCirce = record.asJson
        }
