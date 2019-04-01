## ParseOrElse

Sometimes you're not 100% sure if you can actually parse a given object or not.  For example, let's imagine code you wrote is part of a large, distributed message-passing system.  Your program listens for messages (each corresponding to a case class) and performs actions for some of them.  

Let's further imagine that all messages in this ecosystem derive from trait ActionMessage and that your code is designed to listen for, and respond to, SortActionMessages.

The problem is... you may receive other kinds of messages--messages you don't care about or know what to do with.  You may not even have the jar file containing code to deserialize these other messages!

How will you even deserialize an incoming message to determine if its one of yours?  Hmm.  That's a problem!

This is the scenario parseOrElse() was designed for:  attempt to parse JSON (presumably into a trait).  If successful, very well, but if you can't parse (perhaps because you don't have a class file for the given type) then return a given default object.

```scala
package com.mycompany

case class Wrapped(id:Long, src:String, msg:ActionMessage)
case class DefaultAction() extends ActionMessage // some do-nothing ActionMessage we own

val sj = ScalaJack().parseOrElse(typeOf[ActionMessage] -> typeOf[DefaultAction])

val js = """{"id":123,"src":"main","msg":{"_hint":"com.mycompany.SpecialAction","contact":"fred"}}"""
val myAction = sj.read[Wrapped](js)
```

Presuming we have no code for class SpecialAction, when this Wrapped object is read ScalaJack will consult the ParseOrElse lookup table and see that for an unknown ActionMessage it should return a DefaultAction object.

parseOrElse takes a single mapping Type->Type, where the first argument is the type of the trait to be parsed and the second argument is the type of the default object if the trait's type hint is unknown.  You can supply multiple, chained .parseOrElse() clauses to configure ScalaJack:

```scala
val sj = ScalaJack().parseOrElse(typeOf[A]->typeOf[B]).parseOrElse(typeOf[C}->typeOf[D])
```
