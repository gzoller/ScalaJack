
## Union Types

JSON can be messy. Very messy. Although it is terrible practice, 3rd party JSON of questionable discipline can specify fields that can hold more than one data type.  For example let's assume we get JSON like this from a 3rd party:

```json
{"id":"12345", "category":"linear", "count": 3}
{"id":"12345", "category":["linear", "concentric"], "count": 3}
```
In this example, the category field can either be a String or a List[String]. We can use ScalaJack's support of Scala 3's Union type to solve this problem.

**File 1.scala**
```scala
case class VendorRecord(id: String, category: String | List[String], count: Int)
```
**File 2.scala**
```scala
given sjVendor: ScalaJack[VendorRecord] = sjCodecOf[VendorRecord]

sjVendor.fromJson("""{"id":"12345", "category":"linear", "count": 3}""")
// materializes VendorRecord("12345","linear",3)

sjVendor.fromJson("""{"id":"12345", "category":["linear", "concentric"], "count": 3}""" )
// materializes VendorRecord("12345",List("linear","concentric"),3)
```