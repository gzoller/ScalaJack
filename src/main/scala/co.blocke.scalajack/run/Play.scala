package co.blocke.scalajack
package json
package run

case class Well(repo: scala.collection.mutable.Map[String, Int] = scala.collection.mutable.Map.empty[String, Int])

object RunMe extends App:

  import ScalaJack.*
  import co.blocke.scalajack.json.run.Record

  // given sjPerson: ScalaJack[Person] = sjCodecOf[Person]
  // println(sjPerson.toJson(Person()))

  println(sjCodecOf[Person].toJson(Person()))

  given Well()

  summon[Well].repo.put("a", 1)
  summon[Well].repo.put("b", 2)
  println(summon[Well])


  /* 
  
  given ScalaJack()

  sj[Foo].toJson(myFoo)

  Macro:
    1. Summons SJWell (error if not found)
    2. Looks up codec for Foo. If found, emits a call to it.
    3. If not found, create codec and populate in SJWell
    4. Subsequent calls to sj[Foo] should *NOT* re-gen the macro expansion
  
   */