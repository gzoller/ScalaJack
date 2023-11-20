package co.blocke

import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._

// import io.circe.syntax.*
// import io.circe.*
// import io.circe.generic.semiauto.*
// import io.circe.parser.*

object RunMe extends App:

    // Circe generates a gonzo qty of type information--no actual "code"
    // co.blocke.scalajack.internal.CodePrinter.code {
    //   deriveEncoder[Record]
    // }

    co.blocke.scalajack.internal.CodePrinter.code {
        given codec: JsonValueCodec[Record] = JsonCodecMaker.make
    }
    // given codec: JsonValueCodec[Record] = JsonCodecMaker.make
    // println(readFromString[Record](jsData))

    // println(writeToString(record))

    println("\nDone")