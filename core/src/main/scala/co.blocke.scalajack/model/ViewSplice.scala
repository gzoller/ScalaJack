package co.blocke.scalajack
package model

import typeadapter.CaseClassTypeAdapter
import scala.reflect.runtime.universe._

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
    val viewTarget = taCache.typeAdapter(tt.tpe) match {
      case ta: CaseClassTypeAdapter[_] => ta
      case _ =>
        throw new ScalaJackError(
          s"Output of view() must be a case class.  ${tt.tpe.typeSymbol.fullName} is not a case class."
        )
    }
    val masterData = master.getClass.getDeclaredFields
    val args = viewTarget.fieldMembersByName.toList.flatMap {
      case (fieldName: String, f: ClassHelper.ClassFieldMember[_, _]) =>
        val gotOne = masterData
          .find(
            md => md.getName == f.name && md.getType == f.valueAccessorMethod.getReturnType
          )
          .map(dataField => {
            dataField.setAccessible(true)
            dataField.get(master)
          })
        if (gotOne.isEmpty && !f.isOptional)
          throw new ScalaJackError(
            "View master object " + master.getClass.getName + " is missing field " + fieldName + " required to build view object " + viewTarget.className
          )
        gotOne
    }
    viewTarget.constructorMirror.apply(args: _*).asInstanceOf[T]
  }

  /**
   * Splice a view (subset) object's fields into a master object's fields.
   * @param view the subset object
   * @param master master object
   * @return the master object with the view object's corresponding fields merged/overlayed
   */
  def spliceInto[T, U](view: T, master: U)(implicit tu: TypeTag[U]): U = {
    val masterTarget = taCache.typeAdapter(tu.tpe) match {
      case ta: CaseClassTypeAdapter[_] => ta
      case _ =>
        throw new ScalaJackError(
          s"Output of spliceInto() must be a case class.  ${tu.tpe.typeSymbol.fullName} is not a case class."
        )
    }
    val viewData = view.getClass.getDeclaredFields
    val masterData = master.getClass.getDeclaredFields
    val args = masterTarget.orderedFieldNames.map { fieldName =>
      val f = masterTarget.fieldMembersByName(fieldName)
      viewData
        .find(
          vd => vd.getName == f.name && vd.getType == f.valueAccessorMethod.getReturnType
        )
        .map(dataField => {
          // Found matching master field in view object
          dataField.setAccessible(true)
          dataField.get(view)
        })
        .getOrElse(
          masterData
            .find(_.getName == f.name)
            .map { dataField =>
              // Didn't find matching master field in view object--just use original field from master object
              dataField.setAccessible(true)
              dataField.get(master)
            }
            .get
        )
    }
    masterTarget.constructorMirror.apply(args: _*).asInstanceOf[U]
  }
}
