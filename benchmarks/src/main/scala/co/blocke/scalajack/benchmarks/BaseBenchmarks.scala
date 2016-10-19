package co.blocke.scalajack.benchmarks

import co.blocke.scalajack.{ ScalaJack, HintModifier }
import co.blocke.scalajack.json.Tokenizer
import org.openjdk.jmh.annotations.{ Benchmark, Scope, State }
import scala.reflect.runtime.universe.{ Type, typeOf }

import scala.util.Try

@State(Scope.Benchmark)
class BaseBenchmarksState {

  // I would love for this JSON content to live in a resource, but it appears that JMH cannot access the resources.
  val jsonString = """[{"id":1,"first_name":"Kenneth","last_name":"Watson","email":"kwatson0@goo.ne.jp","gender":"Male","ip_address":"50.27.55.219"},
                     {"id":2,"first_name":"Jason","last_name":"Peters","email":"jpeters1@tinypic.com","gender":"Male","ip_address":"152.156.120.235"},
                     {"id":3,"first_name":"Beverly","last_name":"Stevens","email":"bstevens2@ustream.tv","gender":"Female","ip_address":"169.212.150.35"},
                     {"id":4,"first_name":"Theresa","last_name":"Dixon","email":"tdixon3@hp.com","gender":"Female","ip_address":"137.214.192.32"},
                     {"id":5,"first_name":"Michael","last_name":"Carr","email":"mcarr4@discovery.com","gender":"Male","ip_address":"244.152.168.54"},
                     {"id":6,"first_name":"Carolyn","last_name":"Cruz","email":"ccruz5@nps.gov","gender":"Female","ip_address":"228.112.58.94"},
                     {"id":7,"first_name":"Louis","last_name":"Alexander","email":"lalexander6@mapy.cz","gender":"Male","ip_address":"118.195.8.173"},
                     {"id":8,"first_name":"Laura","last_name":"Campbell","email":"lcampbell7@google.ca","gender":"Female","ip_address":"125.91.1.1"},
                     {"id":9,"first_name":"Judy","last_name":"Burke","email":"jburke8@furl.net","gender":"Female","ip_address":"153.45.26.242"},
                     {"id":10,"first_name":"Earl","last_name":"Stevens","email":"estevens9@discovery.com","gender":"Male","ip_address":"172.161.173.238"},
                     {"id":11,"first_name":"Rose","last_name":"Cooper","email":"rcoopera@lulu.com","gender":"Female","ip_address":"99.128.103.204"},
                     {"id":12,"first_name":"Ashley","last_name":"Hawkins","email":"ahawkinsb@artisteer.com","gender":"Female","ip_address":"128.225.193.155"},
                     {"id":13,"first_name":"Howard","last_name":"Harvey","email":"hharveyc@naver.com","gender":"Male","ip_address":"64.177.55.210"},
                     {"id":14,"first_name":"Edward","last_name":"Ramos","email":"eramosd@is.gd","gender":"Male","ip_address":"208.65.154.100"},
                     {"id":15,"first_name":"Jonathan","last_name":"Gonzalez","email":"jgonzaleze@walmart.com","gender":"Male","ip_address":"166.223.153.41"},
                     {"id":16,"first_name":"Chris","last_name":"Reynolds","email":"creynoldsf@mail.ru","gender":"Male","ip_address":"183.239.230.178"},
                     {"id":17,"first_name":"Helen","last_name":"Morales","email":"hmoralesg@vkontakte.ru","gender":"Female","ip_address":"19.89.226.60"},
                     {"id":18,"first_name":"Tina","last_name":"Baker","email":"tbakerh@hubpages.com","gender":"Female","ip_address":"41.15.68.62"},
                     {"id":19,"first_name":"Patricia","last_name":"Martin","email":"pmartini@booking.com","gender":"Female","ip_address":"98.67.244.69"},
                     {"id":20,"first_name":"Rebecca","last_name":"Kelley","email":"rkelleyj@apple.com","gender":"Female","ip_address":"182.160.172.136"},
                     {"id":21,"first_name":"Bonnie","last_name":"Carr","email":"bcarrk@jigsy.com","gender":"Female","ip_address":"73.181.196.21"},
                     {"id":22,"first_name":"Harold","last_name":"Carter","email":"hcarterl@quantcast.com","gender":"Male","ip_address":"227.72.164.120"},
                     {"id":23,"first_name":"Martha","last_name":"Barnes","email":"mbarnesm@skyrock.com","gender":"Female","ip_address":"46.162.4.230"},
                     {"id":24,"first_name":"Martha","last_name":"Henderson","email":"mhendersonn@quantcast.com","gender":"Female","ip_address":"226.177.120.99"},
                     {"id":25,"first_name":"Ashley","last_name":"Henderson","email":"ahendersono@buzzfeed.com","gender":"Female","ip_address":"159.212.195.202"},
                     {"id":26,"first_name":"Sean","last_name":"Day","email":"sdayp@nationalgeographic.com","gender":"Male","ip_address":"32.29.74.112"},
                     {"id":27,"first_name":"Mary","last_name":"Arnold","email":"marnoldq@sina.com.cn","gender":"Female","ip_address":"217.221.110.62"},
                     {"id":28,"first_name":"Philip","last_name":"Pierce","email":"ppiercer@youtube.com","gender":"Male","ip_address":"170.222.96.245"},
                     {"id":29,"first_name":"Johnny","last_name":"Gordon","email":"jgordons@themeforest.net","gender":"Male","ip_address":"229.207.75.169"},
                     {"id":30,"first_name":"Julie","last_name":"Ruiz","email":"jruizt@jimdo.com","gender":"Female","ip_address":"209.193.34.42"},
                     {"id":31,"first_name":"Benjamin","last_name":"Alvarez","email":"balvarezu@newsvine.com","gender":"Male","ip_address":"69.42.98.157"},
                     {"id":32,"first_name":"Steve","last_name":"Marshall","email":"smarshallv@bizjournals.com","gender":"Male","ip_address":"135.55.106.6"},
                     {"id":33,"first_name":"Aaron","last_name":"Diaz","email":"adiazw@friendfeed.com","gender":"Male","ip_address":"250.102.146.94"},
                     {"id":34,"first_name":"Bonnie","last_name":"Fields","email":"bfieldsx@opera.com","gender":"Female","ip_address":"164.40.128.148"},
                     {"id":35,"first_name":"Beverly","last_name":"Cunningham","email":"bcunninghamy@umn.edu","gender":"Female","ip_address":"4.128.182.77"},
                     {"id":36,"first_name":"Juan","last_name":"Porter","email":"jporterz@nasa.gov","gender":"Male","ip_address":"171.157.112.131"},
                     {"id":37,"first_name":"Donna","last_name":"Butler","email":"dbutler10@cdbaby.com","gender":"Female","ip_address":"126.95.247.209"},
                     {"id":38,"first_name":"Richard","last_name":"Rivera","email":"rrivera11@irs.gov","gender":"Male","ip_address":"219.104.120.129"},
                     {"id":39,"first_name":"Juan","last_name":"Hall","email":"jhall12@ftc.gov","gender":"Male","ip_address":"157.211.238.243"},
                     {"id":40,"first_name":"Heather","last_name":"Lee","email":"hlee13@dailymail.co.uk","gender":"Female","ip_address":"10.153.241.206"},
                     {"id":41,"first_name":"Rose","last_name":"Kennedy","email":"rkennedy14@bravesites.com","gender":"Female","ip_address":"200.54.196.76"},
                     {"id":42,"first_name":"Russell","last_name":"Warren","email":"rwarren15@livejournal.com","gender":"Male","ip_address":"169.251.130.191"},
                     {"id":43,"first_name":"Dennis","last_name":"Howell","email":"dhowell16@biglobe.ne.jp","gender":"Male","ip_address":"222.19.174.168"},
                     {"id":44,"first_name":"Kimberly","last_name":"Wilson","email":"kwilson17@networksolutions.com","gender":"Female","ip_address":"13.139.193.159"},
                     {"id":45,"first_name":"Sharon","last_name":"Jacobs","email":"sjacobs18@stanford.edu","gender":"Female","ip_address":"130.22.68.55"},
                     {"id":46,"first_name":"Donald","last_name":"Nguyen","email":"dnguyen19@posterous.com","gender":"Male","ip_address":"0.115.100.139"},
                     {"id":47,"first_name":"Brenda","last_name":"Stone","email":"bstone1a@senate.gov","gender":"Female","ip_address":"165.196.166.161"},
                     {"id":48,"first_name":"Kelly","last_name":"Pierce","email":"kpierce1b@xrea.com","gender":"Female","ip_address":"73.180.74.227"},
                     {"id":49,"first_name":"Sandra","last_name":"Murray","email":"smurray1c@princeton.edu","gender":"Female","ip_address":"211.149.35.132"},
                     {"id":50,"first_name":"Alice","last_name":"Davis","email":"adavis1d@ow.ly","gender":"Female","ip_address":"4.124.35.181"}]""".stripMargin

  val jsonCharArray = jsonString.toCharArray

  val humanHintMod = new HintModifier {
    def apply(rawHint: String) = rawHint match {
      case "Male"   ⇒ typeOf[Male]
      case "Female" ⇒ typeOf[Female]
    }
    def unapply(hintFieldType: Type) = hintFieldType match {
      case t if (t == typeOf[Male])   ⇒ "Male"
      case t if (t == typeOf[Female]) ⇒ "Female"
    }
  }

  val scalaJack = ScalaJack()
    // .withAdapters(PersonTypeAdapter)
    .withHints((typeOf[Human] -> "gender"))
    .withHintModifiers((typeOf[Human] -> humanHintMod))

  implicit val personFormat = {
    import spray.json._
    import DefaultJsonProtocol._

    jsonFormat6(Person)
  }

  val series4vc = co.blocke.series4.VisitorContext(
    hintMap         = Map("co.blocke.scalajack.benchmarks.Human" → "gender"),
    hintValueRead   = Map("co.blocke.scalajack.benchmarks.Human" → {
      case "Male"   ⇒ new String("co.blocke.scalajack.benchmarks.Male")
      case "Female" ⇒ new String("co.blocke.scalajack.benchmarks.Female")
    }),
    hintValueRender = Map("co.blocke.scalajack.benchmarks.Human" → {
      case "co.blocke.scalajack.benchmarks.Male"   ⇒ new String("Male")
      case "co.blocke.scalajack.benchmarks.Female" ⇒ new String("Female")
    })
  )
  val series4ScalaJack = co.blocke.series4.ScalaJack[String]()

  val listOfPersons = scalaJack.read[List[Person]](jsonString)
}

@State(Scope.Thread)
class BaseBenchmarks {

  //  @Benchmark
  def writePlayJson(state: BaseBenchmarksState): Unit = {
    println(Try { play.libs.Json.stringify(play.libs.Json.toJson(state.listOfPersons)) })
  }

  @Benchmark
  def readHandwritten(state: BaseBenchmarksState): List[Person] = {

    val charArray: Array[Char] = state.jsonCharArray

    val reader = new Tokenizer().tokenize(charArray, 0, charArray.length)

    val listBuilder = List.canBuildFrom[Person]()

    reader.beginArray()

    while (reader.hasMoreElements) {
      reader.beginObject()

      var id: Long = 0L
      var firstName: String = ""
      var lastName: String = ""
      var email: String = ""
      var gender: String = ""
      var ipAddress: String = ""

      while (reader.hasMoreMembers) {
        reader.readString match {
          case "id"         => id = reader.readLong
          case "first_name" => firstName = reader.readString
          case "last_name"  => lastName = reader.readString
          case "email"      => email = reader.readString
          case "gender"     => gender = reader.readString
          case "ip_address" => ipAddress = reader.readString
        }
      }

      listBuilder += Person(id, firstName, lastName, email, gender, ipAddress)

      reader.endObject()
    }

    reader.endArray()

    listBuilder.result()
  }

  @Benchmark
  def readJson4s(state: BaseBenchmarksState): List[Person] = {
    import org.json4s._
    import org.json4s.native.Serialization
    import org.json4s.native.Serialization.{ read, write }
    implicit val formats = org.json4s.DefaultFormats

    //    implicit val formats = Serialization.formats(NoTypeHints)

    //    println(write(state.listOfPersons))

    read[List[Person]](state.jsonString)
  }

  //  @Benchmark
  //  def readLiftJson(state: BaseBenchmarksState): List[Person] = {
  //    import net.liftweb.json._
  //    implicit val formats = DefaultFormats
  //
  //    parse(state.jsonString).extract[List[Person]]
  //  }

  @Benchmark
  def readSpray(state: BaseBenchmarksState): List[Person] = {
    import spray.json._
    import DefaultJsonProtocol._

    import state.personFormat

    state.jsonString.parseJson.convertTo[List[Person]]
  }

  @Benchmark
  def readScalaJack(state: BaseBenchmarksState): List[Person] = {
    state.scalaJack.read[List[Person]](state.jsonString)
    // state.scalaJack.read[List[Person]](state.jsonString)
  }

  //  @Benchmark
  def writeScalaJack(state: BaseBenchmarksState): String = {
    state.scalaJack.render[List[Person]](state.listOfPersons)
  }

  @Benchmark
  def readSeries4ScalaJack(state: BaseBenchmarksState): List[Person] = {
    state.series4ScalaJack.read[List[Person]](state.jsonString, state.series4vc)
  }

  @Benchmark
  def writeSeries4ScalaJack(state: BaseBenchmarksState): String = {
    state.series4ScalaJack.render[List[Person]](state.listOfPersons, state.series4vc)
  }

}
