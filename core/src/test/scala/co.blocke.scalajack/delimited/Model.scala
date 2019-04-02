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
    bd: BigDecimal
)

case class Inside(id: Int, desc: String)
case class Nested(
    thing: String,
    in:    Inside,
    other: Inside = Inside(99, "dunno")
)