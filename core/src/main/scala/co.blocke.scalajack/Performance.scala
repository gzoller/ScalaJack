package co.blocke.scalajack

import co.blocke.scala_reflection._
import co.blocke.scala_reflection.info._
import org.apache.commons.codec.binary.Base64

case class Person(
    id:         Int,
    first_name: String,
    last_name:  String,
    email:      String,
    gender:     String,
    ip_address: String)


/*
object Main {

  val people =
    """[{"id":1,"first_name":"Kenneth","last_name":"Watson","email":"kwatson0@goo.ne.jp","gender":"Male","ip_address":"50.27.55.219"},
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
                   {"id":50,"first_name":"Alice","last_name":"Davis","email":"adavis1d@ow.ly","gender":"Female","ip_address":"4.124.35.181"}]""".stripMargin.asInstanceOf[co.blocke.scalajack.json.JSON]


  val dj = ScalaJack()

  def main(args: Array[String]): Unit = {

    println(RType.of[SampleJNumber])

  /*

    for(i <- 0 to 100)
      dj.read[List[Person]](people)

    val n = 100000
    for (j <- 0 to 30)
      println(s"Run $n iterations in ${run(n)} ms")
      */

  }

  def run(n:Int): Long = 
    val now = System.currentTimeMillis
    for(i <- 0 to n)
      dj.read[List[Person]](people)
    val later = System.currentTimeMillis
    later - now



}
*/

// From reflection:
// ScalaCaseClassInfo(xxx.Thing,class xxx.Thing,List(),List(ScalaFieldInfo(0,t,ArrayInfo([[I,class [[I,ArrayInfo([I,class [I,Scala_Int)),Map(),public int[][] co.blocke.scala_reflection.Thing.t(),None)),List(),Map(),false)

// From scalajack:
// ScalaCaseClassInfo(xxx.Thing,class xxx.Thing,List(),List(ScalaFieldInfo(0,t,ArrayInfo([[I,class scala.Array,ArrayInfo([I,class scala.Array,Scala_Int)),Map(),public int[][] co.blocke.scalajack.Thing.t(),None)),List(),Map(),false)
