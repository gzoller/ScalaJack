## Externalized Types

When parsing some JSON into a Scala trait we use a type hint to know what the concrete case class should be.  Normally this type hint is self-contained within the JSON for the class:
```json
    {"_hint":"com.me.Customer", "name":"Fred", "acct":"abc123"}
```
What if the hint needs to be externalized, i.e. the type is specified *outside* the JSON block for the class?   Perhaps we have a message router that knows how to parse a Message class containing a Payload (trait).  Perhaps the router doesn't need to read the payloads but it does need to know their type to decide where to send the message.  It would be very convenient to "promote" the payload type hint to the outer Message wrapper.

Maybe we need something like this:
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
How would we use this feature?
```scala
    val inst: Message[Command] = Message("abc123", FieldCommand("pong"))
    sj.render(inst)
    // {"payloadKind":"com.me.FieldCommand", "id":"abc123", "payload":{"ping":"pong"}}
```
This is the same render usage as normal, however notice the special treatment of the type hint.  Not only is it outside in the "outer" Message wrapper, but it is called payloadKind, not "_hint".  For external type hints (Scala type members), ScalaJack uses the name of the type member field as the hint.

#### Reading when you have all the class/jar files
Here's an example of reading using a match statement to switch on the kinds of payload:
```scala
val inbound = sj.read[Message[Command]](js)
inbound.payload match {
  case fc:FieldCommand => // stuff here
  case ac:AirCommand => // stuff here
  case _ => // catch-all
}
```
In this case ScalaJack actually materialized the payloads, meaning it needed the class files for those objects.  What if we don't have those jar files?

#### Reading when you *don't* have all the class/jar files
```scala
case class MsgWrapper(kind: String, id: String) extends SJCapture
val inbound = sj.read[MsgWrapper](js)
```
Here we had to create a simplified Message wrapper, that doesn't have a payload field (because we don't have the class files to parse it).  In this example inbound.kind now holds the class name for the payload's type, even though we don't have a corresponding class/jar file for this object.  SJCapture holds all the "extra" payload information so we can safely re-render the whole original object if needed.

__Bottom Line:__ If you specify a type member in your case class, ScalaJack will externalize that type hint, packaging it in the outer or wrapping class. Without a type member, ScalaJack will package the type hint normally inside the serialized class.

### Custom Type Modifiers
Just like for type hints, we may receive 3rd party JSON where having the type value be a fully-qualified Scala class name may not be possible.  We have a limited ability to use the same modifiers we use for type hints.
```scala
val sjm = ScalaJack().withTypeModifier(ClassNameHintModifier((hint: String) => "com.me." + hint, (cname: String) => cname.split('.').last))
val inst: Message[Command] = Message("abc123", FieldCommand("pong"))
val js = sjm.render[Message[Command]](inst)
// {"payloadKind":"FieldCommand", "id":"abc123", "payload":{"ping":"pong"}}
```
Note the class path has been modified and we now only see the trailing class name.  The other out-of-the-box modifier, StringMatchHintModifier, works fine too, in case you need to completely divorce the value in the JSON from any notion of the class name.

**WARNING:** There is a pretty big limitation on type member modifiers:  You can only specify one type modifier globally, i.e. at this time they are not class-specific.  That means the type modifier you specifiy will apply to *all* your type member values!  Perhaps a future version of ScalaJack will support multiple modifiers and make them class/member specific.
