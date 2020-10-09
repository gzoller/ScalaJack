
## scalajack

ScalaJack 7 an all-new implmenation built on Scala 3.  

![Coming Soon](./comingsoon.png)

In your sbt file in settings you'll need to include:  
```
   resolvers += "co.blocke releases resolver" at "https://dl.bintray.com/blocke/releases"
```

To use the (highly-recommended) reflection compiler plug-in, add to build.sbt:
```
addCompilerPlugin("co.blocke" %% "scala-reflection" % VERSION)
```
where VERSION is the latest scala-reflection version found by looking at the Download badge here: [www.blocke.co/scala-reflection](http://www.blocke.co/scala-reflection)

To use:
```scala
import co.blocke.scalajack._

case class Person(name: String, age: Int)

val dj = ScalaJack()
val js = dj.render(Person("Mike",34))  // js == """{"name":"Mike","age":34}"""
val inst = dj.read[Person](js) // re-constitutes original Person
```


### Notes:
* 7.0.0-M1 -- Initial release for Scala 3


### A word about performance...
Compared to pre-7.0 ScalaJack, which used Scala 2.x runtime reflection, ScalaJack is up to 30% faster in many cases when used with the highly-recommended dotty-reflection compiler plugin.  

