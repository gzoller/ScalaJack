
  
## Non-Case and Java Classes

In the last section we saw how easily ScalaJack handles case classes and Scala traits.  If you follow certain conventions, ScalaJack can work with non-case and Java classes too.

An important challenge for ScalaJack is to know what the fields of a class are.  In a case class this is easy: they're given in the constructor.  In a non-case class they *may* be in the constructor--or not.  We can't rely on this, so...

### Scala Non-Case Class val constructor
If you have a non-case Scala class, and you specify 'val' for each of the constructor arguments, ScalaJack will treat it just as a case class.  But... **all** the arguments but be specified with val!

```scala
// Works!
class Employer( val name:String, val est:java.time.LocalDate )

// Doesn't work... not all arguments specified with val
class Employer( name:String, val est:java.time.LocalDate )
```


### Scala Non-Case Class with getters and setters
ScalaJack can optionally detect your non-case class fields using standard Scala getter/setter format with a private var field:

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
When these getter/setter fields are detected they will be serialized, and when such a class is instantiated these fields will be set to their original values by calling the setters.

> If you have any fields with either a getter or setter that you do
> *not* want serialized, put an @Ignore annotation on either the getter or setter and ScalaJack will ignore that field.

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

### Java Class Support
ScalaJack's support for Java classes is more limited due to Java's looser handling of types and constructors.  In order for Scala to detect your class fields your Java class must fit standard JavaBean notation for getters and setters (BeanInfo is used inside ScalaJack to detect your getters and setters).  Your class must also specify a zero-argument constructor.

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