package co.blocke.series4
package json

//
// Be careful about renumbering things in here!  The tokenizing logic uses binary
// shifts (1 or 2 bits) to convert a "standard" token to either one that's an
// element in a list (<<1) or one that's an object key (<<2).  While the JSON
// spec recognizes only Strings as object keys, ScalaJack's "sloppy" mode permits
// non-String keys.
//
object JsonTokens {
  val JSobjStart: Byte = 1
  val JSobjEnd: Byte = 15
  val JSobjEndInList: Byte = 30 // JSobjEnd << 1
  val JSobjEndObjKey: Byte = 60 // JSobjEnd << 2  *WARNING* Non-standard JSON
  val JSlistStart: Byte = 26
  val JSlistEnd: Byte = 7
  val JSlistEndInList: Byte = 14 // JSlistEnd << 1
  val JSlistEndObjKey: Byte = 28 // JSlistEnd << 2  *WARNING* Non-standard JSON
  val JStrue: Byte = 8
  val JStrueInList: Byte = 16 // JStrue << 1
  val JStrueObjKey: Byte = 32 // JStrue << 2 *WARNING* Non-standard JSON
  val JSfalse: Byte = 9
  val JSfalseInList: Byte = 18 // JSfalse << 1
  val JSfalseObjKey: Byte = 36 // JSfalse << 2 *WARNING* Non-standard JSON
  val JSnull: Byte = 10
  val JSnullInList: Byte = 20 // JSnull << 1
  val JSstring: Byte = 11
  val JSstringInList: Byte = 22 // JSstring << 1
  val JSstringObjKey: Byte = 44 // JSstring << 2
  val JSnumber: Byte = 12
  val JSnumberInList: Byte = 24 // JSnumber << 1
  val JSnumberObjKey: Byte = 48 // JSnumber << 2  *WARNING* Non-standard JSON

  // These few are soley used for validation--they never appear in the index
  val JScolon: Byte = 100
  val JScomma: Byte = 101
  val JScommaInList: Byte = 102

  def toName(b: Byte) = b match {
    case JSobjStart      => "JSobjStart"
    case JSobjEnd        => "JSobjEnd"
    case JSobjEndObjKey  => "JSobjEndObjKey"
    case JSobjEndInList  => "JSobjEndInList"
    case JSlistStart     => "JSlistStart"
    case JSlistEnd       => "JSlistEnd"
    case JSlistEndObjKey => "JSlistEndObjKey"
    case JSlistEndInList => "JSlistEndInList"
    case JStrue          => "JStrue"
    case JStrueInList    => "JStrueInList"
    case JStrueObjKey    => "JStrueObjKey"
    case JSfalse         => "JSfalse"
    case JSfalseInList   => "JSfalseInList"
    case JSfalseObjKey   => "JSfalseObjKey"
    case JSnull          => "JSnull"
    case JSnullInList    => "JSnullInList"
    case JSstring        => "JSstring"
    case JSstringInList  => "JSstringInList"
    case JSstringObjKey  => "JSstringObjKey"
    case JSnumber        => "JSnumber"
    case JSnumberInList  => "JSnumberInList"
    case JSnumberObjKey  => "JSnumberObjKey"

    // Used only for validation
    case JScolon         => "JScolon"
    case JScomma         => "JScomma"
    case JScommaInList   => "JScommaInList"
  }
}
