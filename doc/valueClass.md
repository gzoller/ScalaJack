## Value Class Support

ScalaJack supports value classes seamlessly.

```scala
case class UUID_VC(underlying: UUID) extends AnyVal

val u = UUID_VC(UUID.randomUUID)
val js = sj.render(u)
println(js)
// prints: "85c80eb0-5973-4938-abb7-b29b962531ca"

println(sj.read[UUID_VC](js))
// prints: UUID_VC("85c80eb0-5973-4938-abb7-b29b962531ca")
```

You can see here that the wrapping/unwrapping of the value class is handled by ScalaJack and from a JSON perspective the value is treated as though it was never wrapped at all.

Note that if you want custom read/render handling for your underlying type, the process for customization is the same as for the naked type; in other words the fact that the type is wrapped in a value class is immaterial to the customized handling.