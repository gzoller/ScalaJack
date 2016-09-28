package co.blocke.scalajack
package test

case class HooLoo(
  name: String,
  more: HooLoo
)

case class HooLoo2[T](
  name: String,
  x:    T,
  more: HooLoo2[Int]
)

case class HooLoo3[T](
  name: String,
  x:    T,
  more: HooLoo3[T]
)

case class HooLoo4(
  name: String,
  more: Option[HooLoo4]
)

case class HooLoo5(
  name: String,
  more: List[HooLoo5]
)

case class HooLoo6[T](
  name: String,
  x:    T,
  more: List[HooLoo6[T]]
)
