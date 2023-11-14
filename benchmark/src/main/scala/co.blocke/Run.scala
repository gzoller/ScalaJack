package co.blocke


object RunMe extends App:

    import ZIOZ.*
    import zio.json._
    import co.blocke.scalajack.*

    val f = jsData.fromJson[Record]
    println(f)

    println("\n---------")
    println(ScalaJack.write(f))

    println("ZIO Decoder (Address): "+DeriveJsonDecoder.gen[Address])