
## Change Field Names

If you are using ScalaJack with 3rd party JSON you may be in a situation where you don't own or control the names of fields. You may wish the field names of your classes to be different than the names in the 3rd party format. ScalaJack provides the Marking a field with a @Change annotation in your class allows you to change the names of fields in-flight to/from JSON.
  
```scala
case  class  MapFactor(
  @jsLabel("foo_bar") fooBar:String,
  @jsLabel("a_b") thingy: Long,
  count: Int,
  @jsLabel("big_mac") bigMac:String
)
```
If you serialize an instance of this class to JSON you'd get something like:
```JSON
{"foo_bar":"hey","a_b":25,"count":3,"big_mac":"hungry"}
```
Notice that several of these field names are re-mapped to new values, presumably to match the format required by a 3rd party JSON provider.
