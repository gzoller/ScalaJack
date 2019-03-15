package co.blocke.scalajack

// $COVERAGE-OFF$This file is for debugging only!

trait Body
case class FancyBody(message: String) extends Body
case class DefaultBody(message: String = "Unknown body") extends Body
case class AnyBody(stuff: Any) extends Body

trait Hobby
case class InsideHobby(desc: String) extends Hobby

case class Envelope[T <: Body](id: String, body: T) {
  type Giraffe = T
}

// Type member X should be ignored!  Only used internally
case class BigEnvelope[T <: Body, H <: Hobby, X](id: String, body: T, hobby: List[H]) {
  type Giraffe = T
  type Hippo = H
  type IgnoreMe = X

  val x: IgnoreMe = null.asInstanceOf[IgnoreMe]
}

object Runner extends App {

  val sj = ScalaJack()

  val value: BigEnvelope[Body, Hobby, Int] = BigEnvelope("DEF", FancyBody("BOO"), List(InsideHobby("stamps")))
  val js = sj.render[BigEnvelope[Body, Hobby, Int]](value)
  println(js)
  println(sj.read[BigEnvelope[Body, Hobby, Int]](js))

}
// $COVERAGE-ON$

