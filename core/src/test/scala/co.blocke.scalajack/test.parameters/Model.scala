package co.blocke.scalajack
package test
package parameters

//--- Basic Parameterized Case Class
case class Foo1[A](x: A, b: Int)
case class Bar1(name: String)

//--- Advanced Parameterized Case Class
case class Bar2[X](id: X)
case class VC1(s: String) extends AnyVal
case class Foo2[A](x: Bar2[A], b: Int)
case class Foo3[A](x: Bar2[A], b: A)
case class Bar3[X, Y](id: X, isIt: Y)
case class Foo4[A](x: List[Bar3[A, Boolean]], b: A)

//--- Very Advanced Parameterized Case Class
case class Foo5[A, B](x: List[Bar3[A, B]], b: A)
case class Foo6[A, B, C, D](x: Bar4[C, D, A], y: B)
case class Bar4[X, Y, Z](id: X, thing1: Z, thing2: Y)
case class Blah[T, U](t: T, u: U)
case class Foo7[A, B, C, D](x: Bar5[C, D, A], y: B)
case class Bar5[X, Y, Z](id: Y, blah: Blah[Z, X])
