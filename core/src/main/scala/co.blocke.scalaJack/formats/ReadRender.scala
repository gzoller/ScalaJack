package co.blocke.scalajack
package formats

import scala.reflect.runtime.universe._
import scala.reflect.runtime.currentMirror
import PrimitiveTypes._

// Behold, the sublime power of the Cake Pattern.
// The wiring is accomplished in the ScalaJack trait.

// The weird typing ('R' here) is to allow for the fact that future
// serializations may be to non-String output, e.g. bytes.

trait ReadRenderFrame {
	def renderer : ReadRender[_]
	trait ReadRender[R] {
		// This piece of magic handles naked lists, i.e. ScalaJack.render(List(1,2,3)) -- not wrapped in a case class
		def getGraph[T](instance:T)(implicit tt:TypeTag[T]) = {
			val csym = currentMirror.classSymbol(instance.getClass)
			if( csym.isCollection ) { 
				// handle naked collections -- kinda ugly
				val naked = Analyzer.nakedInspect(tt.tpe.typeArgs)
				SjCollection(PrimitiveTypes.fixPolyCollection(csym.fullName).get,naked)
			} else
				Analyzer.inspect(instance) // normal non-collection case
		}
		def render[T](instance:T)(implicit tt:TypeTag[T], vc:VisitorContext=VisitorContext()) : R
	}
}

class RenderException(msg:String) extends Exception(msg)
