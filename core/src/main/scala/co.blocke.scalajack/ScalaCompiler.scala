package co.blocke.scalajack

import scala.reflect.runtime.currentMirror
import scala.tools.reflect.{ FrontEnd, ToolBox }

object ScalaCompiler {

  val frontEnd = new FrontEnd {

    override def display(info: Info): Unit = {
      println(info)
    }

    override def interactive(): Unit = ()

  }

  //  private lazy val toolBox = scala.tools.reflect.ToolBox(currentMirror).mkToolBox()
  private lazy val toolBox = scala.reflect.runtime.universe.runtimeMirror(getClass.getClassLoader).mkToolBox(frontEnd)

  def compileClass(sourceCode: String): Class[_] = {

    println(sourceCode)

    toolBox.eval(toolBox.parse(sourceCode)).asInstanceOf[Class[_]]
  }

}
