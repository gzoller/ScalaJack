package co.blocke.scalajack

import co.blocke.scalajack.json.{ Context, Tokenizer }

import scala.reflect.runtime.universe.typeOf

case class Person(id: Long, first_name: String, last_name: String, email: String, gender: String, ip_address: String)

object FlexJsonSandbox extends App {

  val context = Context.StandardContext

  val personListTypeAdapter = context.typeAdapter(typeOf[List[Person]])

  println(personListTypeAdapter)

  val json = """[{"id":98,"first_name":"Anna","last_name":"Mccoy","email":"amccoy2p@yelp.com","gender":"Female","ip_address":"116.29.53.190"}]"""
  val jsonCharArray = json.toCharArray

  val tokenizer = new Tokenizer
  val jsonReader = tokenizer.tokenize(jsonCharArray, 0, jsonCharArray.length)

  val person = personListTypeAdapter.read(jsonReader)

  println(person)
}
