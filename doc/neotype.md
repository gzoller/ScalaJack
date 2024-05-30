
## NeoType Support

ScalaJack now supports Kit Langton's [excellent neotype library](https://github.com/kitlangton/neotype). Neotype allows you to define validated types that ensure the integrity of the values they contain.

By way of example let's define 3 String-related neotypes:

**File 1.scala**
```scala
//----- A String that must not be empty
type  NonEmptyString  =  NonEmptyString.Type
given  NonEmptyString:  Newtype[String] with
override  inline  def  validate(input: String):  Boolean  =
input.nonEmpty

//----- A String that must be empty
type  EmptyString  =  EmptyString.Type
given  EmptyString:  Newtype[String] with
override  inline  def  validate(input: String):  Boolean  =
input.isEmpty

//----- A List of String that must be non-empty and the first list element must be "x"
type  XList  =  XList.Type
given  XList:  Newtype[List[String]] with
override  inline  def  validate(input: List[String]):  Boolean  =
input.nonEmpty  &&  input(0) ==  "x"

//----- A class to exercise these neotypes
case class Validated(name: NonEmptyString, xspot: XList, nada: EmptyString)
```

For this example, we're going to focus on fromJson().  The toJson() is presumed to always work because the neotype library won't let you create a Validated() object having invalid neotype values.  So the real test here is to see if ScalaJack will correctly detect JSON that violates the defined field validations.

**File 2.scala**
```scala
given sjValidated: ScalaJack[Validated] = sjCodecOf[Validated]

val sj_ok = """{"name":"Mike","xspot":["x","other"],"nada":""}"""
val sj_brokenName = """{"name":"","xspot":["x","other"],"nada":""}"""
val sj_brokenNada = """{"name":"Mike","xspot":["x","other"],"nada":"boom"}"""
val sj_xspot = """{"name":"Mike","xspot":["y","other"],"nada":"boom"}"""

sjValidated.fromJson(sj_ok)
// materializes Validated("Mike",List("x", "other"),"")

try( sjValidated.fromJson(sj_brokenName) )
catch {
  case  jpe: JsonParseError => println(jpe.show)
}
// NeoType validation for NonEmptyString failed at position [9]
// {"name":"","xspot:["x","other"],"nada":""}
// ---------^

try( sjValidated.fromJson(sj_brokenNada) )
catch {
  case  jpe: JsonParseError => println(jpe.show)
}
// NeoType validation for EmptyString failed at position [49]
// {"name":"Mike","xspot":["x","other"],"nada":"boom"}
// -------------------------------------------------^

try( sjValidated.fromJson(sj_xspot) )
catch {
  case  jpe: JsonParseError => println(jpe.show)
}
// NeoType validation for XList failed at position [42]
// {"name":"Mike","xspot":["nintendo","other"],"nada":""}
// ------------------------------------------^
```