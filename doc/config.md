
## ScalaJack Configuration and Usage  
  
### ScalaJack Instantiation  
All ScalaJack usage starts with creating a ScalaJack instance.  
  
```scala  
val sj = ScalaJack()  
```  
  
This instance is used for all serialization activities.  
  
### Flavors  
  
ScalaJack supports several "flavors": JSON, CSV, MongoDB, and DynamoDB out of the box.  Others may create new flavors by extending the pattern.  
  
Here's how to get the right flavor of ScalaJack for your needs:  
  
|Flavor |ScalaJack instantiation  
|-----|--------  
|JSON | val sj = ScalaJack()   // JSON is default if no flavor given  
|CSV  | val sj = ScalaJack(CSVFlavor())  
|MongoDB | val sj = ScalaJack(MongoFlavor())  
|DynamoDB | val sj = ScalaJack(DynamoFlavor())  
  
Note that MongoDB and DynamoDB require a separate library, and are not included in the core ScalaJack package.  
  
### ScalaJack Configuration Methods  
ScalaJack can be configured using "chain" methods, i.e. you chain them together, for example:  
  
```scala  
val sj = ScalaJack()  
  .parseOrElse((typeOf[Person],typeOf[DefaultPerson]))  
  .withDefaultHint("kind")  
```  
Most of these configurations are JSON-only, as they don't make sense for the other formats.  An exception will be thrown if a configuration method is used that isn't supported for a particular ScalaJack flavor.  
    
#### allowPermissivePrimitives()  
Some 3rd party JSON can be pretty messy!  They may make booleans and numbers into strings.  allowPermissivePrimitives() allows ScalaJack to be a little less strict and try to accept "true" as true, for example.  It's a little adaptation to an imperfect world.
  
```scala  
val js = """["true","false"]  
val sj1 = ScalaJack()  
val sj2 = ScalaJack().allowPermissivePrimitives()  
  
sj1.read[List[Boolean]](js) // explodes with error: expected Boolean not String  
  
sj2.read[List[Boolean]](js) // reads List(true,false)  
```

    
#### enumsAsInts()  
You can think of an Enumeration as either a label (String) or an integer (position within the list of allowed values).  When parsing, ScalaJack will try and accept either a String or an Int as a valid value for an Enumeration-typed field.  By default, when rendering, ScalaJack renders the String label.  If you want to render the Int, the set this configuration.
  
```scala  
object Size extends Enumeration {
val Small, Medium, Large = Value
}
case class SampleEnum(e1: Size.Value, e2: Size.Value)
import Size._

val sj1 = ScalaJack()  
val sj2 = ScalaJack().enumsAsInts()  
val inst = SampleEnum(Large, null)  
sj1.render(inst) // renders {"e1":"Large","e2":null}
sj2.render(inst) // renders {"e1":2,"e2":null}
```

#### parseOrElse(poe: (Type, Type)*)  
Configures a default object to be returned if any object of the particular given type can't be parsed successfully.  This is a mapping, so multiple specifications are possible: (match_type, default_object_type)
  
```scala  
val sj = ScalaJack()  
  .parseOrElse( 
    (typeOf[Address], typeof[DefaultAddress]), 
    (typeOf[Command], typeOf[DoNothingCmd]) 
  )  
```  
So in this example, whenever ScalaJack fails to parse an Address-typed value it will substitute a DefaultAddress object in its place.

*__Note:__* There's an important constraint here.  The default objects must have zero-argument constructors because ScalaJack will be trying to create these objects and won't have any arguments to supply.
  
#### withAdapters(ta: TypeAdapterFactory*)  
Register a list of custom TypeAdapters with ScalaJack.  This is to allow you to offer custom serialization handling of your own types.  
  
[See an example.](custom.md)  
  
#### withDefaultHint(hint: String)  
This simply changes the globally default type hint key strings (ScalaJack default hint key is "_hint").  This can be mixed with withHints().  Any type not specified in withHints will pick up your re-set default hint.  
  
#### withHints(h: (Type, String)*)  
Specify per-type hints to override the global default type hint of "_hint".  
__Tip:__ To get the type of a class, for example to use with this function, you can use:  
```scala  
typeOf[MyClass]  
```  
  
[See an example.](typeHint.md)  
  
#### withHintModifiers(hm: (Type, HintModifier)*)  
Where withHints modifies the key String for a type hint, withHintModifiers modifies the hint's value.  Particiularly handy for 3rd party JSON where you don't own/control the values and you want a function to line it up with your internal representation.  
  
[See an example.](typeHint.md)  
  
#### withTypeModifier(tm: HintModifier)  
This is used to apply a hint value modifier (function) the string identifying the externalized type hint (type member) of a class.  This is global, I'm afraid, so any modifier you specify here will be applied to all type members serialized with this instance of ScalaJack.  
  
[See an example.](externalTypes.md)  

