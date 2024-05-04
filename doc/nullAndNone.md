
## Null and None Handling

Representing nulls and None is problematic in JSON because while the spec provides for null it offers nothing to represent None. That forces us to sometimes make inconsistent, contextualized assumptions about how best to represent these concepts. Sadly, there is no perfect "right" answer, so here we document the imperfect compromises ScalaJack made.

Note that in some cases ScalaJack's Null/None handling break the general promise that a read/render round-trip of a serialized object will result in the original object. This is because depending on the handling of Null/None, output may be skipped.

|Usage |Example|JSON Representation
|-------|-------|-------------
|Class member|```MyClass(None,3)```|{"age":3} *(eliminate None field)*
|List|```List(Some(1),None,Some(3))```|[1,3] *(eliminate None value)*
|Map value|```Map("a"->Some(1),"b"->None)```|{"a":1} *(eliminate None value)*
|Map key|```Map(Some(1)->"a",None->"b",Some(2)->"c",None->"d")```|{"1":"a","2":"c"} *(eliminate None values)*
|Tuple member|```(5,None,"hey")```|[5,null,"hey"] *(None converted to null)*

Tuples represent None as null as a compromise. We can't drop the value from the tuple because it has a fixed size that must be respected in the JSON. JSON can't handle an empty field, i.e. "[1,\,3]", so the only choice left is to use null.
  
**Special Note**

If this isn't already confusing, it gets slightly more terrible. Parsing JSON null back into Scala is complicated by the type of field. In a tuple, if the member's type is Option[], a null parses as None, otherwise it parses as null. Just be careful! You may not get exactly the object you expect. Clear as mud?

**Option None and Null**
By default ScalaJack will omit None values in many contexts (see above chart). You can change that behavior and force None values to be converted to null by using a config policy:
```scala
given sjFoo: ScalaJack[Foo] = sjCodecOf[Foo](SJConfig().withNoneAsNull())
```


**Try Failure Value and Null**

By default, ScalaJack will output null for a Try value of Failure.  Alternative handling for Failure is possible via policy:

Try Failure policies are:
* TryPolicy.AS_NULL -- output null upon Failure value (default behavior)
* TryPolicy.ERR_MSG_STRING -- put error string into JSON (**WARNING**: This may corrupt the data type, but is useful for debugging during development)
* TryPolicy.THROW_EXCEPTION -- throw an exception upon Failure value

To set a different policy:
```scala
given sjFoo: ScalaJack[Foo] = sjCodecOf[Foo](SJConfig().withTryFailureHandling(TryPolicy.THROW_EXCEPTION))
```

**Either Left Value and Null**
By default, Scalajack will output the actual value of an Either Left value.  Alternative handling for Left is possible via policy:

Either Left policies are:
* EitherLeftPolicy.AS_VALUE -- output whatever the wrapped Left value is (default behavior)
* EitherLeftPolicy.AS_NULL - output null upon Left value
* EitherLeftPolicy.ERR_MSG_STRING -- put error string into JSON (**WARNING**: This may corrupt the data type, but is useful for debugging during development)
* EitherLeftPolicy.THROW_EXCEPTION -- throw an exception upon Failure value

To set a different policy:
```scala
given sjFoo: ScalaJack[Foo] = sjCodecOf[Foo](SJConfig().withEitherLeftHandling(EitherLeftPolicy.AS_NULL))
```
