package co.blocke.scalajack
package model

// Scala classes should mix this in
trait SJCapture {
  var captured: Option[String] = None // TODO:  Not right--just a placeholder til we re-implement this feature
  //  var captured: Option[IRAndOps] = None
}

// Java classes should inherit this!
class SJCaptureWrapper extends SJCapture