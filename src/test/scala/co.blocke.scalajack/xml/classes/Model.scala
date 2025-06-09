package co.blocke.scalajack
package xml
package classes

@xmlLabel("bouncer")
case class Ball(size: Int)

@xmlLabel("dude")
case class Person(
    name: String,
    @xmlLabel("duration") age: Int,
    @xmlEntryLabel("boo") mine: List[Int],
    pet: Animal
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

sealed trait Animal:
  val legs: Int

case class Dog(legs: Int, food: String) extends Animal
