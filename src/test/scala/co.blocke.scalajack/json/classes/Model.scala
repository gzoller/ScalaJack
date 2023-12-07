package co.blocke.scalajack
package json
package classes

case class Person(name: String, age: Int)

class Parent(phase:Int):
    private var _hidden: Boolean = false
    def hidden: Boolean = _hidden
    def hidden_=(h: Boolean) = _hidden = h

case class Child(name: String, age: Int, phase: Int) extends Parent(phase)

case class Params[X,Y](a: List[X], b: Option[Y])

// Simple case class serialization

// Test inheritance with annotations

// Test non-constructor fields of plain classes (Parent)

// Parameterized classes

// Sealed trait w/case classes
// Sealed trait w/non-case classes
// Sealed trait w/case objects

// Sealed abstract class w/case classes
// Sealed abstract class w/non-case classes
// Sealed abstract class w/case objects

// Java class

// Self-referencing class
// Self-referencing class where self-ref has a parameter Thing[T](a:Int, b: Option[Thing[T]])

//  [*]   [ ] - type hint label mapping
//  [*]   [ ] - type hint value mapping