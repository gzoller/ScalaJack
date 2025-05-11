## Custom Class Codecs

ScalaJack seamlessly renders classes into JSON but sometimes you may wish custom renderings of a class.  Take this class, for example:

```scala
case class PhoneNumber(countryCode: Int, areaCode: Int, prefix: Int, line: Int)
val inst = PhoneNumber(1,123,456,7890)
```

Normal JSON rendering would be:

```json
{"countryCode": 1, "areaCode": 123, "prefix": 456, "line": 7890}
```

ScalaJack allows you to define custom codecs for alternate renderings, like this:

```scala
given JsonCodec[PhoneNumber] = new JsonCodec[PhoneNumber] {  
  val phoneRegex: Regex = raw"\+(\d) \((\d{3})\) (\d{3})-(\d{4})".r  
  def encodeValue(in: PhoneNumber, out: JsonOutput): Unit = 
    out.value(s"+${in.countryCode} (${in.areaCode}) ${in.prefix}-${in.line}")  
  def decodeValue(in: JsonSource): PhoneNumber =  
    in.expectString() match {  
      case phoneRegex(country, area, prefix, line) => 
        PhoneNumber(country.toInt, area.toInt, prefix.toInt, line.toInt)  
      case _                                       => throw new Exception("boom")  
    }  
}
```

Now whenever a PhoneNumber is rendered to JSON it will be shown as:
```
+1 (123) 456-7890
```

**Limitiations**
You may not override implementing classes of a sealed trait. The reason for this is because ScalaJack's logic develops a "fingerprint" of the field names of child class constructor fields and uses this fingerprint to determine if we need to generate and require type hints. If all the possible child classes of a sealed trait are deemed to have unique fingerprints, no type hints are required.

The problem comes when users supply one of these override codecs. There's no way, at compile-time, to know what fields will be emitted in the user's custom code, and thus what the fingerprint should be. Worse, as in this PhoneNumber example, the emitted output isn't required to be a class! So there's no guarantee a fingerprint could be developed at all.

You may have an overridden codec for a field of a child of a trait, like this:

```scala
// OK
sealed trait Person
case class Worker(name: String, age: Int, phone: PhoneNumber) extends Person
```

This however would cause ScalaJack to fail:
```scala
// Bad
sealed trait Contact
case class PhoneNumber(countryCode: Int, areaCode: Int, prefix: Int, line: Int) extends Contact
```
And then you override the codec for PhoneNumber as above. Then when parsing a Contact the parser would be completely confused when it encountered a simple String value for a Contact when it expects a class (or another trait). Even if you rendered PhoneNumber as a class, ie as having fields rather than a primitive type, the fingerprinting logic wouldn't know what fields you specified and would still likely fail badly.  Just don't do it.
