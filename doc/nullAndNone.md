## Null and None Handling

Representing nulls and None is problematic in JSON because while the spec provides for null it offers nothing to represent None.  That forces us to sometimes make inconsistent, contextualized assumptions about how best to represent these concepts.  Sadly, there is no perfect "right" answer here, so these are the compromises ScalaJack made.

Note that in some cases ScalaJack's Null/None handling break the general promise that a read/render round-trip of a serialized object will result in the original object!

|Usage  |Example|JSON Representation
|-------|-------|-------------
|Class member|```MyClass(None,3)```|{"age":3}  *(eliminate None field)*
|List|```List(Some(1),None,Some(3))```|[1,3] *(eliminate None value)*
|Map value|```Map("a"->Some(1),"b"->None)```|{"a":1} *(eliminate None value)*
|Map key|```Map(Some(1)->"a",None->"b",Some(2)->"c",None->"d")```|{"1":"a","2":"c"} *(eliminate None values)*
|Tuple member|```(5,None,"hey")```|[5,null,"hey"] *(None converted to null)*

Tuples represent None as null as a compromise.  We can't drop the value from the tuple because it has a fixed size that must be respected in the JSON.  JSON can't handle an empty field, i.e. "[1,,3]", so the only choice left is to use null.  

**Special Note**
If this isn't already confusing, it gets slightly more terrible.  Parsing JSON null back into Scala is complicated by the type of field.  

In a tuple, if the member's type is Option[], a null parses as None, otherwise it parses as null.

Just be careful!  You may not get exactly the object you expect.  

Clear as mud?
