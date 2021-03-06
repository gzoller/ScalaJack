
## Delimited Format (e.g. CSV)  
  
ScalaJack offers support for delimited serialization (e.g. CSV).  It must be said clearly that there are structural limitations to the support available because delimited format can't support advanced nested structures.  Here are some of the features and limitations imposed by ScalaJack on delimited-format serialization:  
  
* Each input string (presumably a line of a larger input file) shall be parsed to one given object  
* Map structures are not supported (how would you do key-value in a flat format?)  
* Lists are implemented by a nested field, escaped in quotes.  This is handy for very simple lists, but can get messy quickly.  Use with caution!  
* Scala Either is supported but delimited input is very likely to be read as a String, which can mess up Left/Right of an Either if both are String-like
* Enumerations are supported  
* Traits are **not** supported (where would you put the type hint in CSV format?)  
* You must enclose a field with double-qutoes if it contains double quotes, the delimiter character ('comma), or line breaks.  
* Don't try reading Any types!  In delimited input, everything will be interpreted as a String, so if that's not what you want you'll be disappointed.  
* Double quotes within a string/field must be escaped using double-double quotes, "" (not the more JSON style \")  
```scala  
val sj = ScalaJack(DelimitedFlavor)  // CSV default  
// val sj = ScalaJack(DelimitedFlavor('|'))  <-- to set a non-comma delimiter character  
val inst = StringHolder("Hey, you", "This \"\"life\"\"", "And now, \"\"Mike\"\" will sing.")  
val csv = sj.render(inst)  
// renders: "Hey, you","This "life"","And now, "Mike" will sing."  
```  
  
### Lists  
Simple list support is provided.  
```scala  
val sj = ScalaJack(DelimitedFlavor)  
case class Foo(a:Int, b:List[String])  
val f = Foo(1,List("a","b","c"))  
sj.render(f)  
// renders 1,"a,b,c"  
```  
Note that while clever, list representation can get ugly fast.  Consider the case where the strings in the list need to themselves be escaped because they contain a delimiter character.  
```scala  
val f = Foo(1,List("a","b,x","c"))  
sj.render(f)  
// renders 1,"a,""b,x"",c"  
```  
Not awful, but you can see where the double-quotes could multiply very quickly for any more sophisticated structure.  
  
### Handling None and Null  
Empty values in delimited input are read in as null in most cases, with the exception of an Optional field, where a null is read as None.  If a case class field is read as empty, and the field has a defined default value, the default value is returned.

### Takeaway  
Generally it's best to treat delimited format as "fragile", meaning don't get too clever or cute with it.  Use simple, flat or nearly-flat objects and you'll be fine.