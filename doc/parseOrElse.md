
## ParseOrElse and Cascading Fallback Parsing  
  
Sometimes you're not 100% sure if you can actually parse a given object or not.  For example, let's imagine code you wrote is part of a large, distributed message-passing system.  Your program listens for messages (each corresponding to a case class).  Let's further imagine that all messages in this ecosystem derive from trait ActionMessage and that your code is designed to listen for SortAction messages.  The problem is... you may receive other kinds of messages--messages you don't care about or know what to do with.  You may not even have the jar file containing code to deserialize these other messages.  
  
How will you even deserialize an incoming message to determine if it's one of yours?  That's a problem. 
  
This is the scenario parseOrElse() was designed for:  attempt to parse JSON (presumably into a trait).  If successful very well, but if you can't parse (perhaps because you don't have a class file for the given type) then return a given default object.  
  
```scala  
package com.mycompany  
  
trait ActionMessage
case class Wrapped(id:Long, src:String, msg:ActionMessage)  
case class SortAction(isDescending: Boolean) extends ActionMessage
case class DefaultAction() extends ActionMessage // some do-nothing ActionMessage we own  
  
val sj = ScalaJack().parseOrElse(typeOf[ActionMessage] -> typeOf[DefaultAction])  
  
val js = """{"id":123,"src":"main","msg":{"_hint":"com.mycompany.SpecialAction","contact":"fred"}}"""  
val myAction = sj.read[Wrapped](js)  
```  
  
Presuming we have no code for class SpecialAction, when this Wrapped object is read ScalaJack will consult the ParseOrElse lookup table and see that for an unknown ActionMessage it should return a DefaultAction object.  
  
parseOrElse takes a mapping Type->Type, where the first argument is the type of the trait to be parsed and the second argument is the type of the default object if the trait's type hint is unknown.  

### Cascading Fallback

parseOrElse() can be cascaded as in this example:

```scala  
package com.mycompany  
  
trait ActionMessage { val priority: Int }  
case class Wrapped(id: Long, src: String, msg: ActionMessage)  

case class SortAction(priority: Int, isDescending: Boolean) extends ActionMessage  
case class DefaultAction(priority: Int) extends ActionMessage with SJCapture // some do-nothing ActionMessage we own  
case class UnknownMessage() extends ActionMessage with SJCapture {  
  val priority = -1  
}
  
val sj = ScalaJack()  
  .parseOrElse(  
    typeOf[ActionMessage] -> typeOf[DefaultAction],  
  typeOf[DefaultAction] -> typeOf[UnknownMessage]  
  )
  
val js =  
  """{"id":123,"src":"main","msg":{"_hint":"com.mycompany.SpecialAction","priority":2,"contact":"fred"}}""" 
val myAction = sj.read[Wrapped](js)  // Produces DefaultAction

val js2 =  // (missing required priority field for ActionMessage)
  """{"id":123,"src":"main","msg":{"_hint":"com.mycompany.SpecialAction","contact":"fred"}}""" 
val myAction2 = sj.read[Wrapped](js2)  // Produces UnknownMessage
```  

In this case, as before, we attempt to deserialize a class, SpecialAction, for which we have no jar file.  As before we attempt to fall back to deserializing a DefaultAction, but in this case that fails too, because the required 'priority' field is missing from the second input (js2).  In this case we fall back again and create an UnknownMessage.

As an added flourish we've mixed in SJCapture so that all the data sent in the JSON is at least captured, even if we don't known what to do with it.  That's purely optional, of course, if you need that data.

The intention of parseOrElse() along with the cascading fallback pattern is that you shouldn't need a nest of Exception handlers.  You should be able to always have your parse produce something rational and useful.