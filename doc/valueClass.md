
## Value Class Support

ScalaJack supports value classes seamlessly.

 **File1.scala**
```scala
case  class  UUID_VC(underlying: UUID) extends  AnyVal
```

**File2.scala**
```scala
given sjUUID: ScalaJack[UUID_VC] = sjCodecOf[UUID_VC]
val  u = UUID_VC(UUID.randomUUID)
val  js = sjUUID.toJson(u)
println(js) // prints: "85c80eb0-5973-4938-abb7-b29b962531ca"
println(sjUUID.fromJson(js)) // prints: UUID_VC("85c80eb0-5973-4938-abb7-b29b962531ca")
```
You can see here that the wrapping/unwrapping of the value class is handled by ScalaJack and from a JSON perspective the value is treated as though it was never wrapped at all.
