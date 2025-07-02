
## Raw Support

There are times when you might want to encapsulate raw chunks of JSON within a structured JSON document. In other words you may have a situation
where you want to partially parse meaningful parts of JSON structure and capture, but not parse, the rest. Think of a class supporting some kind
of message router, say a Packet. Packet may have several fields you care about and specify normally as fields in a case class. Paket may also have
a payload, which you don't really care about--you're merely transporting. You may not even know anything about the structure of that payload
other than it is well-formed JSON. This is when ScalaJack's Raw support is useful.

```scala
case  class  Packet(id: String, src: Long, dest: Long, payload: JsonRaw)
```

We can use JsonRaw to parse JSON like the following:
```scala
val msg = """{"id":"abc123", "src":12345, "dest":67890, payload: {"some":"interesting","stuff":"here"}}"""

val sj = sjCodecOf[Packet]
val packet = sj.fromJson(msg)
``` 
This code will parse msg into the Packet fields. Field payload will remain a JsonRaw. If you were to unwrap the payload you would see the 
underlying type (String) value, which would contain the raw json object """{"some":"interesting","stuff":"here"}""".

Serialization works the same way, in reverse of course:
```scala
val rendered = sj.toJson(packet)
```


***Note: For now this is a JSON-only feature. We may decide to implement an XML equivalent if the need arises. ***