package co.blocke.scalajack
package benchmarks

import org.openjdk.jmh.annotations.{ Benchmark, Scope, State }
import scala.reflect.runtime.universe.{ Type, typeOf }
import co.blocke.scalajackx.hybrid._

import scala.util.Try

@State(Scope.Benchmark)
class BaseBenchmarksState {

  //--------------- Series X ScalaJack Setup
  val h_intTypeAdapter = IntTypeAdapter(IntJsonSerializer())
  val h_arrayTypeAdapter = ListTypeAdapter[Int](ArrayJsonSerializer(h_intTypeAdapter))
  val h_arrayTypeAdapter2 = ListTypeAdapter[List[Int]](ArrayJsonSerializer(h_arrayTypeAdapter))

  //--------------- Series 6 ScalaJack Setup
  val series6ScalaJack = ScalaJack()

  //--------------- Series 6.X ScalaJack Setup
  val series6X = series6ScalaJack.forType[List[List[Int]]]

  //--------------- Series 5 ScalaJack Setup
  val humanHintModSeries5 = new co.blocke.series5.HintModifier {
    def apply(rawHint: String) = rawHint match {
      case "Male"   => typeOf[Male]
      case "Female" => typeOf[Female]
    }
    def unapply(hintFieldType: Type) = hintFieldType match {
      case t if (t == typeOf[Male])   => "Male"
      case t if (t == typeOf[Female]) => "Female"
    }
  }

  implicit val personFormat = {
    import spray.json._
    import DefaultJsonProtocol._
    jsonFormat6(Person)
  }

  val series5ScalaJack = co.blocke.series5.ScalaJack()
    //    .withAdapters(PersonTypeAdapter_Series5)
    .withHints((typeOf[Human] -> "gender"))
    .withHintModifiers((typeOf[Human] -> humanHintModSeries5))

  //--------------- Series 4 ScalaJack Setup
  val series4vc = co.blocke.series4.VisitorContext(
    hintMap         = Map("co.blocke.scalajack.benchmarks.Human" -> "gender"),
    hintValueRead   = Map("co.blocke.scalajack.benchmarks.Human" -> {
      case "Male"   => new String("co.blocke.scalajack.benchmarks.Male")
      case "Female" => new String("co.blocke.scalajack.benchmarks.Female")
    }),
    hintValueRender = Map("co.blocke.scalajack.benchmarks.Human" -> {
      case "co.blocke.scalajack.benchmarks.Male"   => new String("Male")
      case "co.blocke.scalajack.benchmarks.Female" => new String("Female")
    })
  )
  val series4ScalaJack = co.blocke.series4.ScalaJack[String]()
}

@State(Scope.Thread)
class BaseBenchmarks {

  //  import play.api.libs.json._
  //  @Benchmark
  //  def writePlayJson(state: BaseBenchmarksState): Unit = {
  //    println(Try { Json.stringify(Json.toJson(state.listOfPersons)) })
  //  }

  @Benchmark
  def readPrototype(state: BaseBenchmarksState): List[List[Int]] = {
    val ps = JsonParserState("[[1,2,3,4,5],[1,2,3,4,5],[1,2,3,4,5]]")
    val prim = state.h_arrayTypeAdapter2.serializer.toPrimitives(state.h_arrayTypeAdapter2.serializer.parse(ps))
    state.h_arrayTypeAdapter2.materialize(prim)
  }

  @Benchmark
  def readSeries6ScalaJack(state: BaseBenchmarksState): List[List[Int]] = {
    state.series6ScalaJack.read[List[List[Int]]]("[[1,2,3,4,5],[1,2,3,4,5],[1,2,3,4,5]]")
  }

  @Benchmark
  def readSeries6XScalaJack(state: BaseBenchmarksState): List[List[Int]] = {
    state.series6X.fastRead("[[1,2,3,4,5],[1,2,3,4,5],[1,2,3,4,5]]")
    //    val series6TA = state.series6ScalaJack.context.typeAdapterOf[List[List[Int]]] // This line is causing the slowness!
    //    val ps = co.blocke.scalajack.json.JsonParserState("[[1,2,3,4,5],[1,2,3,4,5],[1,2,3,4,5]]")
    //    val prim = series6TA.serializer.toPrimitives(state.series6TA.serializer.parse(ps))
    //    series6TA.materialize(prim)
    //    val prim = state.series6TA.serializer.toPrimitives(state.series6TA.serializer.parse(ps))
    //    state.series6TA.materialize(prim)
  }

  @Benchmark
  def readSeries5ScalaJack(state: BaseBenchmarksState): List[List[Int]] = {
    state.series5ScalaJack.read[List[List[Int]]]("[[1,2,3,4,5],[1,2,3,4,5],[1,2,3,4,5]]")
    //    state.series5ScalaJack.read[List[Person]](state.jsonString)
  }

  @Benchmark
  def readSeries4ScalaJack(state: BaseBenchmarksState): List[List[Int]] = {
    state.series4ScalaJack.read[List[List[Int]]]("[[1,2,3,4,5],[1,2,3,4,5],[1,2,3,4,5]]", state.series4vc)
    //    state.series4ScalaJack.read[List[Person]](state.jsonString, state.series4vc)
  }
}
