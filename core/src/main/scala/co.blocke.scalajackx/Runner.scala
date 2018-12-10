package co.blocke.scalajackx

object Runner extends App {

  val js = "[[1,2,3,4,5],[1,2,3,4,5],[1,2,3,4,5]]"
  val iterations = 1 //s1000000

  val ser = json4s.JsonSerializer()

  val prim = ser.parseToPrimitive(js)

  println(prim)

  //------ Json4s Native parsing + values (flattening JValue structures)

  val ita = json4s.IntTypeAdapter()
  val lta = json4s.ListTypeAdapter[Int](ita)
  val llta = json4s.ListTypeAdapter[List[Int]](lta)

  val one = timer(() => {
    for (x <- 1 to iterations) {
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
    for (x <- 1 to iterations) {
      val ps = uroll.JsonParserState(js)
      arrayTypeAdapter2.parser.materialize(arrayTypeAdapter2.parser.parse(ps))
    }
  })
  println("U-Roll: " + two)

  //------ SJ Parsing + Json4s flattening
  val h_intTypeAdapter = hybrid.IntTypeAdapter(hybrid.IntJsonSerializer())
  val h_arrayTypeAdapter = hybrid.ListTypeAdapter[Int](hybrid.ArrayJsonSerializer(h_intTypeAdapter))
  val h_arrayTypeAdapter2 = hybrid.ListTypeAdapter[List[Int]](hybrid.ArrayJsonSerializer(h_arrayTypeAdapter))

  val three = timer(() => {
    for (x <- 1 to iterations) {
      val ps = hybrid.JsonParserState(js)
      val prim = h_arrayTypeAdapter2.serializer.toPrimitives(h_arrayTypeAdapter2.serializer.parse(ps))
      val s = h_arrayTypeAdapter2.materialize(prim)
      //      h_arrayTypeAdapter2.serializer.emit(h_arrayTypeAdapter2.dematerialize(s), (new StringBuilder()).asInstanceOf[h_arrayTypeAdapter2.serializer.EmitterState])
    }
  })
  println("Hybrid: " + three)

  //------ SJ Series 6 (unreleased)
  import co.blocke.scalajack._

  val sj = ScalaJack()

  val five = timer(() => {
    for (x <- 1 to iterations) {
      val y = sj.read[List[Set[Int]]](js.asInstanceOf[sj.WIRE])
      println(y)
    }
  })
  println("SJ 6: " + five)

  //-------------------------
  def timer(fn: () => Unit): Long = {
    val now = System.currentTimeMillis()
    fn()
    val later = System.currentTimeMillis()
    return (later - now)
  }
}
