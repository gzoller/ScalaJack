package co.blocke.series4

import scala.reflect.runtime.universe._

trait ViewSplice {
  /**
   * Project fields from given master object to a view object of type T.  Field names/types must match master
   * precisely.
   * @param master the master object from which the smaller object is projected
   * @return an object of type T which is a "subset" of the master
   */
  def view[T](master: Any)(implicit tt: TypeTag[T]): T =
    Analyzer.inspectByName(tt.tpe.typeSymbol.fullName) match {
      case viewClass: CCType =>
        val masterData = master.getClass.getDeclaredFields
        val args = viewClass.members.collect {
          case (fname, ftype) => masterData.find(_.getName == fname).map(tf => {
            tf.setAccessible(true)
            (fname, tf.get(master))
          })
        }.flatten.toMap
        viewClass.materialize(args).asInstanceOf[T]
      case _ => throw new ViewException("Type parameter must be a case class, but was instead " + tt.tpe.typeSymbol.fullName)
    }

  /**
   * Splice a view (subset) object's fields into a master object's fields.
   * @param view the subset object
   * @param master master object
   * @return the master object with the view object's corresponding fields merged/overlayed
   */
  def spliceInto[T, U](view: T, master: U)(implicit tu: TypeTag[U]): U =
    Analyzer.inspectByName(tu.tpe.typeSymbol.fullName) match {
      case masterClass: CCType =>
        val viewData = view.getClass.getDeclaredFields
        val masterData = master.getClass.getDeclaredFields
        val args = masterClass.members.collect {
          case (fname, ftype) => viewData.find(_.getName == fname).map(tf => {
            tf.setAccessible(true)
            (fname, tf.get(view))
          }).orElse(masterData.find(_.getName == fname).map(tf => {
            tf.setAccessible(true)
            (fname, tf.get(master))
          }))
        }.flatten.toMap
        masterClass.materialize(args).asInstanceOf[U]
      case _ => throw new ViewException("Type parameter must be a case class, but was instead " + tu.tpe.typeSymbol.fullName)
    }
}

case class ViewException(msg: String) extends Exception(msg)