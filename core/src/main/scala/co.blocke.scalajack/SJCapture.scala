package co.blocke.scalajack

trait SJCapture {
  var captured: java.util.HashMap[String,_] = new java.util.HashMap[String, Any]()
  // var captured: java.util.HashMap[String,String] = new java.util.HashMap[String, String]()
}

// Java classes should inherit this!
class SJCaptureJava extends SJCapture
