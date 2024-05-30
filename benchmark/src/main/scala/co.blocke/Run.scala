package co.blocke

import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._

object RunMe extends App:

  import co.blocke.scalajack.ScalaJack.*
  import co.blocke.scalajack.*

  //case class MapHolder( m: Map[Pet2, String])
  // val mh = MapHolder( Map(Pet2("Mindy","Frenchie",4)->"a", Pet2("Rosie","Terrier",8)->"b"))
  // given codec: JsonValueCodec[Pet2] = JsonCodecMaker.make
  // given codec2: JsonValueCodec[MapHolder] = JsonCodecMaker.make
  // println(writeToString(mh))
  
  println("\nDone")