package co.blocke.scalajack

trait SJCapture {
  var captured: Option[JsonAndOps] = None
}

// Java classes should inherit this!
class SJCaptureWrapper extends SJCapture