## Filter

Earlier in this documentation we saw a number of ways to process input through ScalaJack, including ways using SJCapture and externalized type hints to build filtering capabilities.  Wouldn't it be nice if there was a cleaner, Scala-like way of filtering on a type?

```scala
trait Comm  
case class Event(happening: Int) extends Comm  
trait Command extends Comm { val goDo: String }  
case class SimpleCommand(goDo: String, public: Boolean) extends Command  
case class CommMessage[T <: Comm](id: Int, payload: T) {  
 type kind = T
 }
 
val js = "123"
val isMap = sj.filter[Map[String, Int]]()  
val isInt = sj.filter[Int]()  
val isCmd = sj.filter[CommMessage[Command]]("kind")  
val isEvt = sj.filter[CommMessage[Event]]("kind")

sj.parse(js) match {  
 case isMap(x) => println(x) 
 case isInt(x) => println("Int: " + x) 
 case isCmd(x) => println("Cmd: " + x) 
 case isEvt(x) => println("Evt: " + x) 
 case _          => 
 }
```
sj.filter() returns a Filter object that can be used as a type-safe Scala extractor.

Nice!
