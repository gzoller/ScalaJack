## Externalized Types

When parsing some JSON into a Scala trait we use a type hint to know what the concrete case class should be.  Normally this type hint is self-contained within the JSON for the class:
```json
    {"_hint":"com.me.Customer", "name":"Fred", "acct":"abc123"}
```
What if the hint needs to be externalized, i.e. the type is specified *outside* the JSON block for the class?  Maybe we need something like this:
```scala
    package com.me
    
    trait Command
    case class FieldCommand(ping:String) extends Command
    
    case class Message[T <: Command](id:String, payload:T)
```
Let's further assume the JSON we want would look like this:
```json
    {"payloadKind":"com.me.FieldCommand", "id":"abc123", "payload":{"ping":"pong"}}
```
Notice that the type hint is outside the object it describes (payload).  How can we accomplish this?  How can we move the type hint outside the described object?

ScalaJack uses a type member in your class to specify a type hint of a contained object.  Like this:
```scala
    case class Message[T <: Command](id:String, payload:T) {
      type payloadKind = T
    }
```
So how would we use this feature?
```scala
    val inst: Message[Command] = Message("abc123", FieldCommand("pong"))
    sj.render[Message[Command]](inst)
    // {"payloadKind":"com.me.FieldCommand", "id":"abc123", "payload":{"ping":"pong"}}
```
Basically this is the same render usage as normal, except we have to specify a type Message[Command] when calling render()--normally you don't need this.  Scala's type system needs a little bit of help when using type members this way.

Reading is basically the same, again using a type in the call to read().  Here's an example of reading using a match statement to switch on the kinds of payload:
```scala
val inbound = sj.read[Message[Command]](js)
inbound.payload match {
  case fc:FieldCommand => // stuff here
  case ac:AirCommand => // stuff here
  case _ => // catch-all
}
```
