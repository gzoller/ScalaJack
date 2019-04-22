package co.blocke.scalajack
package delimited

case class AllPrim(
    i:  Int,
    l:  Long,
    d:  Double,
    f:  Float,
    c:  Char,
    s:  String,
    b:  Boolean,
    bi: BigInt,
    bd: BigDecimal)

case class Inside(id: Int, desc: String)
case class Nested(
    thing: String,
    in:    Inside,
    other: Inside = Inside(99, "dunno"))

case class ThreeStrings(a: String, b: String, c: String)
case class DefaultThree()

case class WithList[T](id: Int, someList: List[T])

case class HasOption(one: Option[Int], two: Option[Int] = Some(5))

case class HasTuples(one: (String, Int), two: (Boolean, Int) = (true, 1))
case class HasTuples2(one: (String, Inside))

case class HasEither(one: Int, two: Either[Int, Inside])
case class HasEither2(one: Int, two: Either[Int, Inside] = Right(Inside(1, "ok")))
case class HasEither3(one: Int, two: Either[String, Inside])

object Size extends Enumeration {
  val Small, Medium, Large = Value
}
case class Shirt(id: Int, size: Size.Value)
case class Shirt2(id: Int, size: Either[Size.Value, Int])
