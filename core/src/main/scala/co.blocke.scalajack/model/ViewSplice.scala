package co.blocke.scalajack
package model

import typeadapter.classes.CaseClassTypeAdapter

trait ViewSplice {

  me: JackFlavor[_] =>

  /**
   * Project fields from given master object to a view object of type T.  Field names/types must match master
   * precisely.
   * @param master the master object from which the smaller object is projected
   * @return an object of type T which is a "subset" of the master
   */
  // WARNING: Assumes CaseClassTypeAdapter.members is in constructor-order.  If not, sort on members.index.
  def view[T](master: Any)(implicit tt: TypeTag[T]): T = {
    val viewTarget = context.typeAdapter(tt.tpe) match {
      case ta: CaseClassTypeAdapter[_] => ta
      case _                           => throw new ViewException(s"Output of view() must be a case class.  ${tt.tpe.typeSymbol.fullName} is not a case class.")
    }
    val masterData = master.getClass.getDeclaredFields
    val args = viewTarget.fieldMembersByName.flatMap {
      case (fname, f) =>
        val gotOne = masterData.find(md => md.getName == f.name && md.getType == f.valueAccessorMethod.getReturnType).map(dataField => {
          dataField.setAccessible(true)
          dataField.get(master)
        })
        if (gotOne.isEmpty && !f.isOptional)
          throw new ViewException("View master object " + master.getClass.getName + " is missing field " + fname + " required to build view object " + viewTarget.className)
        gotOne
    }.toList
    viewTarget.constructorMirror.apply(args: _*).asInstanceOf[T]
  }

  /**
   * Splice a view (subset) object's fields into a master object's fields.
   * @param view the subset object
   * @param master master object
   * @return the master object with the view object's corresponding fields merged/overlayed
   */
  def spliceInto[T, U](view: T, master: U)(implicit tu: TypeTag[U]): U = {
    val masterTarget = context.typeAdapter(tu.tpe) match {
      case ta: CaseClassTypeAdapter[_] => ta
      case _                           => throw new ViewException(s"Output of spliceInto() must be a case class.  ${tu.tpe.typeSymbol.fullName} is not a case class.")
    }
    val viewData = view.getClass.getDeclaredFields
    val masterData = master.getClass.getDeclaredFields
    val args = masterTarget.fieldMembersByName.map {
      case (_, f) =>
        viewData.find(vd => vd.getName == f.name && vd.getType == f.valueAccessorMethod.getReturnType).map(dataField => {
          // Found matching master field in view object
          dataField.setAccessible(true)
          dataField.get(view)
        }).getOrElse(masterData.find(_.getName == f.name).map { dataField =>
          // Didn't find matching master field in view object--just use original field from master object
          dataField.setAccessible(true)
          dataField.get(master)
        }.get)
    }.toList
    masterTarget.constructorMirror.apply(args: _*).asInstanceOf[U]
  }
}
