package co.blocke.scalajack
package xml

import co.blocke.scala_reflection.reflect.rtypeRefs.*

// Support annotation @jsLabel to change field names
private inline def changeFieldName(fr: FieldInfoRef): String = fr.annotations.get("co.blocke.scalajack.xmlLabel").flatMap(_.get("name")).getOrElse(fr.name)
