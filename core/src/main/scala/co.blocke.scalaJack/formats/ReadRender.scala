package co.blocke.scalajack
package formats

import scala.reflect.runtime.universe._

// Behold, the sublime power of the Cake Pattern.
// The wiring is accomplished in the ScalaJack trait.

// The weird typing ('R' here) is to allow for the fact that future
// serializations may be to non-String output, e.g. bytes.

trait ReadRenderFrame {
	def renderer : ReadRender[_]
	trait ReadRender[R] {
		def render[T](instance:T)(implicit tt:TypeTag[T]) : R
	}
}