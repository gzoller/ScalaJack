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

#### Custom Type Modifiers
Just like for type hints, we may receive 3rd party JSON where having the type value be a fully-qualified Scala class name may not be possible.  We have a limited ability to use the same modifiers we use for type hints.
```scala
val sjm = ScalaJack().withTypeModifier(ClassNameHintModifier((hint: String) => "com.me." + hint, (cname: String) => cname.split('.').last))
val inst: Message[Command] = Message("abc123", FieldCommand("pong"))
val js = sjm.render[Message[Command]](inst)
// {"payloadKind":"FieldCommand", "id":"abc123", "payload":{"ping":"pong"}}
```
Note the class path has been modified and we now only see the trailing class name.  The other out-of-the-box modifier, StringMatchHintModifier, works here too, in case you need to completely divorce the value in the JSON from any notion of the class name.

**WARNING:** There is currently two pretty big limitations on type modifiers.  
 -  You can only specify one type modifier.
 -  The type modifier will apply to *all* your type member values!

So if you use type members in your case classes only for the purpose of externalizing your type hints, *and* you don't care if all these type hints are processed the same way, then this is a simple and effective way of modifying the value to something other than a fully-qualified class name.

Perhaps a future version of ScalaJack will support multiple modifiers and make them class/member specific.
