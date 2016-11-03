## Non-Case and Java Classes

In the last section we saw how easily ScalaJack handles case classes and Scala traits.  It can handle non-case classes and Java classes too.  The Scala case class is a fantastic thing, and does a lot behind the scenes on your behalf, but if you follow certain conventions, ScalaJack can work with a non-case class.

The big challenge for ScalaJack is to know what the members, aka fields, of a class are.  In a case class this is easy: they're given in the constructor.  In a non-case class they *may* be in the constructor--or not.

#### Scala Non-Case Class val constructor
If you have a non-case Scala class, and you specify 'val' for each of the constructor arguments, ScalaJack will treat it just as a case class.  But... **all** the arguments but be specified with val!

```scala
// Works!
class Employer( val name:String, val est:java.time.LocalDate )

// Doesn't work... not all arguments specified with val
class Employer( name:String, val est:java.time.LocalDate )
```

#### Scala Non-Case Class with public var
Another way to specify your class fields is to define them as public var fields.
```scala
class Employer() {
  var name:String = ""
  var est:LocalDate = LocalDate.now()
  var profitMargin:Double = 21.0
}
```

ScalaJack finds these fields and treats them as class fields.

Oops!  We have a problem.  ScalaJack's auto-detection of all var fields means that *all* public var fields are treated as class members!  This may not be what we want, so ScalaJack provides an @Ignore annotation to let you ignore var fields you don't want detected and treated as class members:

```scala
class Employer() {
  var name:String = ""
  var est:LocalDate = LocalDate.now()
  @Ignore var profitMargin:Double = 21.0
}
```

#### Scala Non-Case Class with getters and setters
Yet another way ScalaJack can detect your class fields using standard Scala getter/setter format:

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
It turns out Scala creates the getters and setters for you automatically, which means you can use a hybrid of public vars and getter/setters to let ScalaJack detect your class fields:

```scala
class Employer() {
  var name:String = ""
  
  private var _est:LocalDate = LocalDate.now()
  def est: LocalDate = _est
  def est_=(e:LocalDate) = _est = e
}
```

#### Java Class Support
ScalaJack's support for Java classes is more limited.  In order for Scala to detect your class fields your Java class must fit standard JavaBean notation for getters and setters (BeanInfo is used inside ScalaJack to detect your getters and setters):

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
