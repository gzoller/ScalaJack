package co.blocke.scalajack

import scala.reflect.runtime.currentMirror
import scala.tools.reflect.{ FrontEnd, ToolBox }

object ScalaCompiler {

  //  private lazy val toolBox = scala.tools.reflect.ToolBox(currentMirror).mkToolBox()

  def compileClass(sourceCode: String): Class[_] = {

    val lines: IndexedSeq[String] = sourceCode.split('\n')

    val frontEnd = new FrontEnd {

      override def display(info: Info): Unit = {
        val lineNumber = info.pos.line
        val startLineNumber = (lineNumber - 3) max 1
        val endLineNumber = (lineNumber + 3) min lines.size

        val stringBuilder = new StringBuilder

        for ((line, lineNumber) <- lines.slice(startLineNumber - 1, endLineNumber - 1) zip (startLineNumber to endLineNumber)) {
          val prefix = s"[$lineNumber]: "
          stringBuilder.append(prefix).append(line).append("\n")
          if (lineNumber == info.pos.line) {
            stringBuilder.append(" " * (prefix.length + info.pos.column - 1)).append("^\n")
          }
        }

        println(info)
        println(stringBuilder.result())
      }

      override def interactive(): Unit = ()

    }

    val toolBox = scala.reflect.runtime.universe.runtimeMirror(getClass.getClassLoader).mkToolBox(frontEnd)

    println(sourceCode)

    toolBox.eval(toolBox.parse(sourceCode)).asInstanceOf[Class[_]]
  }

}
