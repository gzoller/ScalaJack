package co.blocke.scalajack.run

case class Person(name: String, age: Int, isOk: List[Boolean], favColor: Colors)

enum Colors:
  case Red, Blue, Green
