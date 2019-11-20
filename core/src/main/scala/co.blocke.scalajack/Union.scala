package co.blocke.scalajack

trait _Union // marker trait

case class Union2[A, B](a: Option[A], b: Option[B]) extends _Union {
  def unpack: Any = a.getOrElse(b.get)
  def _unpack: (Any, Int) = a.map(v => (v, 0)).getOrElse((b.get, 1))
}

case class Union3[A, B, C](a: Option[A], b: Option[B], c: Option[C])
  extends _Union {
  def unpack: Any = a.getOrElse(b.getOrElse(c.get))
  def _unpack: (Any, Int) =
    a.map((_, 0)).getOrElse(b.map((_, 1)).getOrElse((c.get, 2)))
}

case class Union4[A, B, C, D](
    a: Option[A],
    b: Option[B],
    c: Option[C],
    d: Option[D])
  extends _Union {
  def unpack: Any = a.getOrElse(b.getOrElse(c.getOrElse(d.get)))
  def _unpack: (Any, Int) =
    a.map((_, 0))
      .getOrElse(b.map((_, 1)).getOrElse(c.map((_, 2)).getOrElse((d.get, 3))))
}
