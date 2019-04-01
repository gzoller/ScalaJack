
## Non-Case and Java Classes

In the last section we saw how easily ScalaJack handles case classes and Scala traits.  The Scala case class is a fantastic thing, and does a lot behind the scenes on your behalf, but if you follow certain conventions, ScalaJack can work with non-case and Java classes too.

The big challenge for ScalaJack is to know what the fields of a class are.  In a case class this is easy: they're given in the constructor.  In a non-case class they *may* be in the constructor--or not.  We can't rely on this, so...

### Scala Non-Case Class val constructor (most preferred method)
If you have a non-case Scala class, and you specify 'val' for each of the constructor arguments, ScalaJack will treat it just as a case class.  But... **all** the arguments but be specified with val!

```scala
// Works!
class Employer( val name:String, val est:java.time.LocalDate )

// Doesn't work... not all arguments specified with val
class Employer( name:String, val est:java.time.LocalDate )
```


### Scala Non-Case Class with getters and setters (ok method)
Another way ScalaJack can detect your class fields using standard Scala getter/setter format with a private var field:

```scala
class Employer() {
  private var _name:String = ""
  def name: String = _name
  def name_=(n:String) = _name = n

  private var _est:LocalDate = LocalDate.now()
  def est: LocalDate = _est
  def est_=(e:LocalDate) = _est = e
}
```

If you have any fields with either a getter or setter that you do *not* want serialized, put an @Ignore annotation on either the getter or setter and ScalaJack will ignore that field.

### Scala Non-Case Class with public var (least preferred method)
A final way to specify your Scala class fields is to define them as public var fields, which is bad for the obvious reason of violating data hiding.  ScalaJack finds these fields and treats them as class fields.

```scala
class Employer() {
  var name:String = ""
  var est:LocalDate = LocalDate.now()
  var profitMargin:Double = 21.0
}
```

ScalaJack's auto-detection of all var fields means that *all* public var fields are treated as class members!  This may not be what you want, so ScalaJack provides an @Ignore annotation to let you ignore var fields you don't want detected and treated as class members:

```scala
class Employer() {
  var name:String = ""
  var est:LocalDate = LocalDate.now()
  @Ignore var profitMargin:Double = 21.0
}
```

As you know, exposing all var fields like this is very poor practice, so we strongly recommend against using the public vars method of field detection.

### Default Values and Option
With any of these methods it is possible to supply default values

```scala
class Employer( val label:String = "ACME" )  {
  private var _name:String = "Fred"
  @Optional def name: String = _name
  def name_=(n:String) = _name = n

  @Optional var num: Int = 5
}
```
Here you can see the defaults "ACME", "Fred", and 5 supplied as defaults to the detectable class members.  But what's that @Optional annotation?  Scala requires a value be provided for non-constructor vars (or the class would be abstract).  ScalaJack assumes all fields detected are required, so if a parsed class is missing a field, say name, it wouldn't know whether that's a missing-field error, or whether it should just use "Fred".  The @Optional annotation tells ScalaJack that the field is optional in the parsed input, and if it's missing, just use the given value.

So why not just declare @Optional fields with Scala Option?  What happens if you have Option type value with a default?

```scala
  private var _name:Option[String] = Some("Fred")
  def name: Option[String] = _name
  def name_=(n:Option[String]) = _name = n
```
In this case, if name was missing from the parsed input, ScalaJack would always read its value as None.  What if I want the default Some("Fred")?  Using @Optional will work:

```
```scala
  private var _name:Option[String] = Some("Fred")
  @Optional def name: Option[String] = _name
  def name_=(n:Option[String]) = _name = n
```
@Optional gives you more control over how fields are parsed.

### Java Class Support
ScalaJack's support for Java classes is more limited due to Java's looser handling of types and constructors.  In order for Scala to detect your class fields your Java class must fit standard JavaBean notation for getters and setters (BeanInfo is used inside ScalaJack to detect your getters and setters):

```java
public class PlayerJava {
   private String name;
   private int age;

   public String getName() { return name; }
   public void setName(String n) { name = n; }
   public int getAge() { return age; }
   public void setAge(int a) { age = a; }
}
```
As with Scala fields, you can also use the @Ignore annotation on either a getter or setter if you don't want a JavaBean field serialized.
