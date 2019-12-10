package co.blocke.scalajack
package json.parameters

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

//--- Basic Parameterized Trait
trait T1[X] { val x: X }
case class TFoo1[A](x: A, b: Int) extends T1[A]
trait T2 { val name: String }
case class TBar1(name: String) extends T2

//--- Advanced Parameterized Trait
trait T3[X] { val thing: X }
case class TBar2(thing: Boolean) extends T3[Boolean]
case class TBar3[T](thing: T) extends T3[T]
trait T4[X] { val x: TBar3[X] }
case class TFoo2[A](x: TBar3[A], b: A) extends T4[A]
trait T5[X, Y] { val thing1: X; val thing2: Y }
case class TBar4[T](thing1: T, thing2: String) extends T5[T, String]
trait T6[X] { val x: List[T5[X, String]] }
case class TFoo3[A](x: List[T5[A, String]]) extends T6[A]

//--- Very Advanced Parameterized Trait
trait T7[X, Y] { val x: T5[X, Y]; val b: X }
case class TBar5[T, U](thing1: T, thing2: U) extends T5[T, U]
case class TFoo4[A, B](x: T5[A, B], b: A) extends T7[A, B]

trait T8[W, X, Y, Z] { val x: T9[Y, Z, W]; val y: X }
trait T9[T, U, V] { val pi: T; val po: U; val pu: V }
case class TBar6[A, B, C](pi: A, po: B, pu: C) extends T9[A, B, C]
case class TFoo5[A, B, C, D](x: T9[C, D, A], y: B) extends T8[A, B, C, D]

// Foo[A,B,C,D](x:Bar[C,Blah[D,A]], y:B)
trait T10[X, Y] { val x: X; val y: Y }
trait T11[W, Z] { val w: W; val z: Z }
case class TBlah1[A, B](w: A, z: B) extends T11[A, B]
case class TBar7[A, B](thing1: A, thing2: B) extends T5[A, B]
case class TFoo6[A, B, C, D](x: T11[C, T5[D, A]], y: B)
  extends T10[T11[C, T5[D, A]], B]
