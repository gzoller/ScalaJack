## CSV

ScalaJack offers CSV serialization support, but it must be said clearly that there are structural limitations to the support available:

* Class parameters must be flat (scalars only)!  CSV has no ability to represent structured or nestable content like collections.
* Traits are not supported (where would you put the type hint in CSV format?)
* You must enclose a field with double-qutoes if it contains double quotes, commas, or line breaks.
* Double quotes within a string/field must be escaped using double-double quotes, "" (not the more common \")
```scala
val sj = ScalaJack(CSVFlavor())
val inst = StringHolder("Hey, you", "This \"life\"", "And now, \"Mike\" will sing.")
val csv = sj.render(inst)
// renders: "Hey, you","This ""life""","And now, ""Mike"" will sing."
```

###Handling None and Null
If an object has a field with value None, then ScalaJack will renders an empty field.  Note this is different than an empty String.  Consider the following CSV creation:

```scala
case class Maybe(one:String, two:Option[String], three:Boolean)

val sj = ScalaJack(CSVFlavor())
val inst = Maybe("yes", Some("This,test"), true)
val csv = sj.render(inst)
// renders: yes,"This,test",true

val inst2 = Maybe("yes", Some(""), true)
val csv2 = sj.render(inst2)
// renders: yes,"",true

val inst3 = Maybe("yes", None, true)
val csv3 = sj.render(inst3)
// renders: yes,,true

val inst4 = Maybe(null, null, true)
val csv4 = sj.render(inst4)
// renders: ,,true
```

**Warning:** Did you catch that last one?  Null and None are both rendered as empty fields.  This means if you try to read this:

```scala
val csv = ",,true"
val materialized = sj.read[Maybe](csv)
// Maybe(null,None,true)
```
*The Option empty field becomes a None*--there's no way to represent a null value for an Optional value in CSV.

For other values empty CSV fields becomes null.

|Field Value |Option |String |
|------|-------|-----|
|""|Some("")  |""
|(empty)|None |null  

