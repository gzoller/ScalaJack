package co.blocke.scalajack
package json


import scala.quoted.*
import scala.collection.mutable
import co.blocke.scala_reflection.{RTypeRef, TypedName}
import writing.JsonOutput


class CodecBuildContext(using val quotes: Quotes):
  val canonicalRefCache: mutable.Map[TypedName, RTypeRef[?]] = mutable.Map.empty
  val seenBefore: mutable.Map[TypedName, Boolean] = mutable.Map.empty
  val generatedWriters: mutable.Map[TypedName, Expr[(Any, JsonOutput) => Unit]] = mutable.Map.empty
  val currentlyGenerating: mutable.Set[TypedName] = mutable.Set.empty
