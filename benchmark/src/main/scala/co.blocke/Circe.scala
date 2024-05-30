package co.blocke

import org.openjdk.jmh.annotations._

object CirceZ:
    import io.circe.syntax.*
    import io.circe.*
    import io.circe.generic.semiauto.*
    import io.circe.parser.*
    
    implicit val recordDecoder: Decoder[Record2] = deriveDecoder[Record2]
    implicit val recordEncoder: Encoder[Record2] = deriveEncoder[Record2]

    implicit val personDecoder: Decoder[Person2] = deriveDecoder[Person2]
    implicit val personEncoder: Encoder[Person2] = deriveEncoder[Person2]

    implicit val addressDecoder: Decoder[Address2] = deriveDecoder[Address2]
    implicit val addressEncoder: Encoder[Address2] = deriveEncoder[Address2]

    implicit val friendDecoder: Decoder[Friend2] = deriveDecoder[Friend2]
    implicit val friendEncoder: Encoder[Friend2] = deriveEncoder[Friend2]

    implicit val petDecoder: Decoder[Pet2] = deriveDecoder[Pet2]
    implicit val petEncoder: Encoder[Pet2] = deriveEncoder[Pet2]

    trait CirceReadingBenchmark{
        @Benchmark
        def readRecordCirce = parse(jsData2).flatMap(_.as[Record2])
        }

    trait CirceWritingBenchmark { 
        @Benchmark
        def writeRecordCirce = record.asJson
        }
