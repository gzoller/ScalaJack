package co.blocke.scalajackx

import co.blocke.scalajack.ScalaJack

object Runner extends App {

  val sj = ScalaJack()

  /*
  p1()
  println("---------")
  p2()

  def p1(): Unit = {
    import parser1._
    val intTypeAdapter = IntTypeAdapter(JsonIntParser())
    //    val booleanTypeAdapter = BooleanTypeAdapter(JsonBooleanParser())
    val arrayTypeAdapter = ListIntTypeAdapter(JsonArrayParser(intTypeAdapter))
    val arrayTypeAdapter2 = ListListIntTypeAdapter(JsonArrayParser(arrayTypeAdapter))

    //    val ps = JsonParserState("12345 67890")
    //    val bs = JsonParserState("true false")
    //    val as = JsonParserState("[1,2,3,4,5]")
    //    val as2 = JsonParserState("[[1,2,3,4,5],[1,2,3,4,5],[1,2,3,4,5]]")

    println("Simple (parser1) ==>")

    val one = timer(() => {
      for (x <- 1 to 1000000) {
        val ps = JsonParserState("12345")
        intTypeAdapter.parse(ps)
      }
    })

    val two = timer(() => {
      for (x <- 1 to 1000000) {
        sj.read[Int]("12345")
      }
    })

    println("   X: " + one)
    println("  v5: " + two)

    println("\nMulti-Array (parser1) ==>")

    val three = timer(() => {
      for (x <- 1 to 1000000) {
        val ps = JsonParserState("[[1,2,3,4,5],[1,2,3,4,5],[1,2,3,4,5]]")
        arrayTypeAdapter2.parse(ps)
      }
    })
    val four = timer(() => {
      for (x <- 1 to 1000000) {
        sj.read[List[List[Int]]]("[[1,2,3,4,5],[1,2,3,4,5],[1,2,3,4,5]]")
      }
    })

    println(" X: " + three)
    println("v5: " + four)
  }

  def p2(): Unit = {
    import parser2._
    val intTypeAdapter = IntTypeAdapter(JsonIntParser())
    //    val booleanTypeAdapter = BooleanTypeAdapter(JsonBooleanParser())
    val arrayTypeAdapter = ListIntTypeAdapter(JsonArrayParser(intTypeAdapter))
    val arrayTypeAdapter2 = ListListIntTypeAdapter(JsonArrayParser(arrayTypeAdapter))

    //    val ps = JsonParserState("12345 67890")
    //    val bs = JsonParserState("true false")
    //    val as = JsonParserState("[1,2,3,4,5]")
    //    val as2 = JsonParserState("[[1,2,3,4,5],[1,2,3,4,5],[1,2,3,4,5]]")

    println("Simple (parser2) ==>")

    val one = timer(() => {
      for (x <- 1 to 1000000) {
        val ps = JsonParserState("12345")
        intTypeAdapter.parser.materialize(intTypeAdapter.parser.parse(ps))
      }
    })

    val two = timer(() => {
      for (x <- 1 to 1000000) {
        sj.read[Int]("12345")
      }
    })

    println("   X: " + one)
    println("  v5: " + two)

    println("\nMulti-Array (parser2) ==>")

    val three = timer(() => {
      for (x <- 1 to 1000000) {
        val ps = JsonParserState("[[1,2,3,4,5],[1,2,3,4,5],[1,2,3,4,5]]")
        arrayTypeAdapter2.parser.materialize(arrayTypeAdapter2.parser.parse(ps))
      }
    })
    val four = timer(() => {
      for (x <- 1 to 1000000) {
        sj.read[List[List[Int]]]("[[1,2,3,4,5],[1,2,3,4,5],[1,2,3,4,5]]")
      }
    })

    println(" X: " + three)
    println("v5: " + four)
    /*
    import parser2._
    val intTypeAdapter = IntTypeAdapter(JsonIntParser())
    val booleanTypeAdapter = BooleanTypeAdapter(JsonBooleanParser())
    val arrayTypeAdapter = ListIntTypeAdapter(JsonArrayParser(intTypeAdapter))

    val three = timer(() => {
      for (x <- 1 to 1000000) {
        val bs = JsonParserState("true")
        booleanTypeAdapter.parser.materialize(booleanTypeAdapter.parser.parse(bs))
      }
    })
    val four = timer(() => {
      for (x <- 1 to 1000000) {
        sj.read[Boolean]("true")
      }
    })
    println(" X: " + three)
    println("v5: " + four)
    */
  }

  def timer(fn: () => Unit): Long = {
    val now = System.currentTimeMillis()
    fn()
    val later = System.currentTimeMillis()
    return (later - now)
  }
  */

}
