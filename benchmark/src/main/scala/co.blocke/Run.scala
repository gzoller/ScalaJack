package co.blocke

import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._

object RunMe extends App:

    given codec: JsonValueCodec[Record] = JsonCodecMaker.make
    println(readFromString[Record](jsData))

    println(writeToString(record))

    println("\nDone")