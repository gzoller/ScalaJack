package co.blocke.scalajack

import scala.tools.reflect.{FrontEnd, ToolBox}

object ScalaCompiler {

  def compileClass(sourceCode: String): Class[_] = {
    val classLoader = Thread.currentThread().getContextClassLoader

    val frontEnd = new FrontEnd {

      override def display(info: Info): Unit = {
        println(info)
      }

      override def interactive(): Unit = ()

    }

    val toolBox = scala.reflect.runtime.currentMirror.mkToolBox(frontEnd)

    toolBox.eval(toolBox.parse(sourceCode)).asInstanceOf[Class[_]]
  }

}
