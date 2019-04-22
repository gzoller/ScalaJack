## Custom Type Adapters

ScalaJack does  a great job of reading and rendering stock data types, but sometimes you just need something custom.  Let's use an example.  Imagine you have a phone number of type String that you want formatted like a US phone number (XXX-XXX-XXXX) but stored as a simple String (no dashes).

To do this ScalaJack allows you to create a custom type adapter and link it into its own chain of adapters.  Let's walk through the process step-by-step.

### Step 1: Create a Type

```scala
object MyTypes {
  type Phone = String
}
import MyTypes._
```

In this case we create a Phone type to differentiate Phone, which is a String, from any other String value.

### Step 2: Create the TypeAdapter
There are 3 essential functional pieces to a ScalaJack TypeAdapter.
1. Something that matches the type we want
2. Something to read input of that type
3. Something to output an object of that type

Let's look at a straightforward example then unpack some nuances.

```scala
object PhoneAdapter extends TypeAdapter.===[Phone] with Stringish {  
  def read[WIRE](path: Path, reader: Reader[WIRE], isMapKey: Boolean): Phone =
    reader.readString(path) match {  
      case s: String => s.replaceAll("-", "")  
      case null => null  
  }  
  
  def write[WIRE](t: Phone, writer: Writer[WIRE], out: Builder[WIRE, WIRE], isMapKey: Boolean): Unit = t match {  
    case null => writer.writeNull(out)  
    case _    => writer.writeString("%s-%s-%s".format(t.substring(0, 3), t.substring(3, 6), t.substring(6)), out)  
  }  
}
```
Here you'll see all three essential pieces.  The type matching is accomplished with ```extends TypeAdapter.===[Phone]```.  The read and write functions speak for themselves.  Note the use of WIRE here (not, for example, JSON).  This is because ScalaJack is a general-purpose serializer so we don't presume JSON.  Each "flavor" of ScalaJack (JSON being one flavor) implements the Reader and Writer traits, which define a nice set of primitive operations we can use for our TypeAdapters.

In this example we see that read() strips out all the dashes from the phone numbers, while write() re-inserts them in a US format.

Phone numbers are Strings, so they're nullable, so we handle null for read/write as well.  (Most of your TypeAdapters will also be nullable unless you are wrapping a non-nullable type like Int.)

Also note the "Stringish" mixin in our TypeAdapter.  This tells ScalaJack that your type is String-encoded.  This is an oddment of Map key handling for WIRE formats like JSON.  Map/Object keys in JSON must be Strings, yet Scala imposes no such limitation.  Therefore ScalaJack must wrap some primitive types in String quotes when used as JSON map keys.  String-encoded (Stringish) types, like Phone, require no such special handling so we notate that in the TypeAdapter to avoid double quoting.  For example:

```scala
sj.render(Map(true -> true, false -> false))
// renders string-wrapped map keys: {"true":true,"false":false}
```

### Step 3: Create the PhoneAdapter

```scala
// Override just Phone
object PhoneAdapter extends TypeAdapter.===[Phone] {
  override val irTransceiver: IRTransceiver[Phone] = new PhoneIRTransceiver()
}
```
You can see we specify our IRTransceiver.  One thing that may not be clear is the Typeadapter.===[Phone] notation.  This matches exactly on Phone type, so ScalaJack doesn't confuse other String values with Phone and try to serialize/deserialize them with your custom code.

If what you want is to treat Phone and all subclasses as Phone (with your custom code), then extend TypeAdapter.=:=[Phone] instead. If we did that in this case, every String would be treated as a Phone, with likely dissastrous results.


### Step 3: Wire your PhoneAdapter into ScalaJack's list of type adapters

To use your new type adapter, hook it into ScalaJack:

```scala
val sj = ScalaJack().withAdapters(PhoneAdapter)
```
Now anything you parse with a Phone in it will receive your specified special handling via your custom adapter.

**TIP:** withAdapters() is varargs, so you can pass a chain of adapter: ScalaJack().withAdapters(a,b,c,...)

### Nuance: Matching Types
When creating custom TypeAdapters in ScalaJack you have a number of options when deciding how to match Types.  ScalaJack basically works like this: it looks for a give type (reading or writing) by iterating through a list of TypeAdapterFactories (with Type matchers) until one matches.  *How* types are matched is the question.

In the Phone example above we used a simple way to create a TypeAdapterFactory using:
```scala
object PhoneAdapter extends TypeAdapter.===[Phone] with Stringish { 
..
}
```
ScalaJack allows 3 kinds of type comparisons when matching using this method:

```scala
type Phone = String  
trait Foo  
class Bar() extends Foo  
```
|Comparator|Meaning
|-------|-------|
|A === B|Type A exactly matches another.  Phone === String is false
|A =:= B|Type A can be implicitly converted to type B.  Phone =:= String is true
|A <:< B|Type A is a subtype of type B.   Bar <:< Foo is true

These 3 comparators will likely provide most of your needed type matching, but if you have a really special case you can write your own custom TypeAdapterFactory.  The TypeAdapterFactory class provides a number of matchings by implementing typeAdapterOf[T].  The CaseClassTypeAdapterFactory class in the ScalaJack code is a good example of usage.
