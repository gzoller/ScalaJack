package co.blocke.scalajackx

object Runner extends App {

  val js = "[[1,2,3,4,5],[1,2,3,4,5],[1,2,3,4,5]]"

  val ser = json4s.JsonSerializer()

  val prim = ser.parseToPrimitive(js)

  println(prim)

  //------ Json4s Native parsing + values (flattening JValue structures)

  val ita = json4s.IntTypeAdapter()
  val lta = json4s.ListTypeAdapter[Int](ita)
  val llta = json4s.ListTypeAdapter[List[Int]](lta)

  val one = timer(() => {
    for (x <- 1 to 1000000) {
      val prim = ser.parseToPrimitive(js)
      llta.read(prim)
    }
  })
  println("Json4s: " + one)

  //------ SJ Parsing + SJ value flattening

  val intTypeAdapter = uroll.IntTypeAdapter(uroll.JsonIntParser())
  val arrayTypeAdapter = uroll.ListTypeAdapter[Int](uroll.JsonArrayParser(intTypeAdapter))
  val arrayTypeAdapter2 = uroll.ListTypeAdapter[List[Int]](uroll.JsonArrayParser(arrayTypeAdapter))

  val two = timer(() => {
    for (x <- 1 to 1000000) {
      val ps = uroll.JsonParserState("[[1,2,3,4,5],[1,2,3,4,5],[1,2,3,4,5]]")
      arrayTypeAdapter2.parser.materialize(arrayTypeAdapter2.parser.parse(ps))
    }
  })
  println("U-Roll: " + two)

  //------ SJ Parsing + Json4s flattening
  val h_intTypeAdapter = hybrid.IntTypeAdapter(hybrid.JsonIntParser())
  val h_arrayTypeAdapter = hybrid.ListTypeAdapter[Int](hybrid.JsonArrayParser(h_intTypeAdapter))
  val h_arrayTypeAdapter2 = hybrid.ListTypeAdapter[List[Int]](hybrid.JsonArrayParser(h_arrayTypeAdapter))

  val three = timer(() => {
    for (x <- 1 to 1000000) {
      val ps = hybrid.JsonParserState("[[1,2,3,4,5],[1,2,3,4,5],[1,2,3,4,5]]")
      val prim = h_arrayTypeAdapter2.parser.toPrimitives(h_arrayTypeAdapter2.parser.parse(ps))
      h_arrayTypeAdapter2.materialize(prim)
    }
  })
  println("Hybrid: " + three)

  //-------------------------
  def timer(fn: () => Unit): Long = {
    val now = System.currentTimeMillis()
    fn()
    val later = System.currentTimeMillis()
    return (later - now)
  }
}
