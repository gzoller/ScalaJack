package co.blocke.scalajack
package xml
package classes

@xmlLabel("bouncer")
case class Ball(size: Int)

@xmlLabel("dude")
case class Person(
    name: String,
    @xmlLabel("duration") age: Int,
    @xmlEntryLabel("bop") mine: List[Ball]
) //, @xmlEntryLabel("bip")items: Map[String,Int])

/*
So at this point we have:
 * Simple case classes
 * Primmitive data types
 * Lists
 * Label and entry renaming
 * Maps -- in-progress

Next:
 * Option (empty elements)
 */
