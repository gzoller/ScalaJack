ScalaJack is a simple, very fast JSON parser/renderer for Scala case classes.

It supports unaided introspection of Scala case classes (i.e. no annotations or helper code required).
ScalaJack is very fast as it's built on top of Jackson, but uses just enough Scala reflection to
support JSON serialization without helpers.

Some differentiating features:

- Arbitrary combinations of List, Map, and Option are possible (n-levels deep) for complex structures
- Supports value classes
- MongoDB DBObject render/read support including compound keys
- Support for traits (e.g. a data value of type trait Animal will resolve implementing classes Dog and Cat)
- Render/read custom (dynamic!) JSON for value classes
- Supports type parameters for case classes and traits
- View/SpliceInto feature for "lite" versions of larger objects
- Very fast!

Online at:

- Code: [source](https://github.com/gzoller/ScalaJack)

