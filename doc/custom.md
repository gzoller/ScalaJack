## Custom Type Adapters

ScalaJack does  a great job of reading and rendering data types, but sometimes you just need something custom.  Let's use an example.  Imagine you have a phone number of type String that you want formatted like a US phone number (XXX-XXX-XXXX) but stored as a simple String (no dashes).

To do this ScalaJack allows you to create a custom type adapter and link it into its own chain of adapters.

```scala
object MyTypes {
  type Phone = String
}
import MyTypes._

object PhoneAdapter extends BasicTypeAdapter[Phone] {
  override def read(reader: Reader): Phone = {
    reader.peek match {
      case TokenType.String =>
        val raw = reader.readString()
        raw.replaceAll("-", "").asInstanceOf[Phone]
      // "%s-%s-%s".format(raw.substring(0, 3), raw.substring(3, 6), raw.substring(6)).asInstanceOf[Phone]
      case TokenType.Null =>
        reader.readNull()
    }
  }

  override def write(value: Phone, writer: Writer): Unit =
    if (value == null) {
      writer.writeNull()
    } else {
      writer.writeString("%s-%s-%s".format(value.substring(0, 3), value.substring(3, 6), value.substring(6)))
      // writer.writeString(value.replaceAll("-", ""))
    }
}
```

There are a few things going on here.  First we define a type to differentiate Phone from String (unless you want all Strings to be formatted as phone numbers).  You'll see we have to implement a read and write method for our custom adapter.  This is pretty straightforward formatting to/from JSON.  Don't forget to handle null if your base type is nullary, which String is.

You can cut 'n paste this example for a lot of simple uses.  Reader and Writer classes are good references.

To use your new type adapter, hook it into ScalaJack:

```scala
val sj = ScalaJack().withAdapters(PhoneAdapter)
```

Now anything you parse with a Phone in it will receive special handling via your custom adapter.

**TIP:** withAdapters() is varargs, so you can pass a chain of adapter: ScalaJack().withAdapters(a,b,c,...)

#### Advanced
The example above was the trivial case, which is going to be all you need most of the time.  For those rare occasions when more is needed, here are some further details.

ScalaJack's internal processing is devided into 2 stages: a reflection/analyze stage, and a runtime read/write stage.  ScalaJack maintains a chain of TypeAdapterFactory which, given a type, traverses the chain looking for a factory that serves that type.  The factory produces a type-specific TypeAdapter.

There are two general-purpose TypeAdapterFactory implementations: BasicTypeAdapter, and SimpleTypeAdapter.  All primitives use these.  The key difference between the two is how types are matched during ScalaJack's analyze traversal stage.

BasicTypeAdapter will match only if the type is *exactly* the one given, making it ideal for most customizations.  For example Phone type should only match Phone.  SimpleTypeAdapter uses a different type comparator that matches a type *and all its supertypes*.  So if our custom example above was based on SimpleTypeAdapter and you gave it type Phone, it would match for all Strings too!  Probably not what you intended--but maybe in some cases that *is* what you need.

If you really have special needs, you can always write a full-up TypeAdapter implementation vs using one of the two general-purpose TypeAdapters.  Please refer to the typeadapter package for examples of all kinds of different use cases.

However you create your TypeAdapters, you wire them into ScalaJack the same way as shown above.  Your custom adapters are put in the front of the chain, superseding any of the default adapters, so you can use this process to override out-of-the-box behavior.