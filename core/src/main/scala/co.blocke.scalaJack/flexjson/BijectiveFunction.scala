package co.blocke.scalajack.flexjson

object BijectiveFunction {

  def apply[A, B](apply: A ⇒ B, unapply: B ⇒ A): BijectiveFunction[A, B] =
    BijectiveFunctionPair(apply, unapply)

  object Implicits {

    implicit final class FunctionReverse[A, B](private val apply: A ⇒ B) extends AnyVal {
      @inline def reversedBy(unapply: B ⇒ A): BijectiveFunction[A, B] = BijectiveFunction(apply, unapply)
      def ⇄(unapply: B ⇒ A): BijectiveFunction[A, B] = reversedBy(unapply)
    }

  }

}

trait BijectiveFunction[A, B] extends Function[A, B] {

  override def apply(a: A): B

  def unapply(b: B): A

  def inverse: BijectiveFunction[B, A] =
    InvertedBijectiveFunction(this)

  def compose[X](f: BijectiveFunction[X, A]): BijectiveFunction[X, B] =
    ComposedBijectiveFunction(f, this)

  def andThen[C](g: BijectiveFunction[B, C]): BijectiveFunction[A, C] =
    ComposedBijectiveFunction(this, g)

  def memoized: BijectiveFunction[A, B] =
    MemoizedBijectiveFunction(this)

}

case class BijectiveFunctionPair[A, B](
  applyFn:   A ⇒ B,
  unapplyFn: B ⇒ A
) extends BijectiveFunction[A, B] {

  override def apply(a: A): B = applyFn(a)

  override def unapply(b: B): A = unapplyFn(b)

}

case class InvertedBijectiveFunction[A, B](f: BijectiveFunction[A, B]) extends BijectiveFunction[B, A] {

  override def apply(b: B): A = f.unapply(b)

  override def unapply(a: A): B = f.apply(a)

  override def inverse: BijectiveFunction[A, B] = f

}

case class ComposedBijectiveFunction[A, B, C](
  f: BijectiveFunction[A, B],
  g: BijectiveFunction[B, C]
) extends BijectiveFunction[A, C] {

  override def apply(a: A): C = g.apply(f.apply(a))

  override def unapply(c: C): A = f.unapply(g.unapply(c))

}

case class MemoizedBijectiveFunction[A, B](f: BijectiveFunction[A, B]) extends BijectiveFunction[A, B] {

  import scala.collection.mutable

  val applyCache = new mutable.WeakHashMap[A, B]
  val unapplyCache = new mutable.WeakHashMap[B, A]

  override def apply(a: A): B =
    applyCache.getOrElseUpdate(a, f.apply(a))

  override def unapply(b: B): A =
    unapplyCache.getOrElseUpdate(b, f.unapply(b))

  override def memoized: BijectiveFunction[A, B] =
    this

}
