package co.blocke.scalajack

trait SJCapture {
  var captured: java.util.HashMap[String, _] =
    new java.util.HashMap[String, Any]()
}

// Java classes should inherit this!
class SJCaptureJava extends SJCapture
