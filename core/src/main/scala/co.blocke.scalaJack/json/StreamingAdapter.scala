package co.blocke.scalajack
package json

import java.io.InputStreamReader

import java.io._

case class StreamingAdapter(in: InputStreamReader, BLOCK_SIZE: Int = 262144)(implicit tokenizer: JsonTokenizer) {
  private var data = new Array[Char](BLOCK_SIZE * 3)
  private var dataLen = 0
  private var pos = 0

  def nextObject(): Option[JsonIndex] = {
    if (dataLen == 0) // first parse...load buffer from source
      appendBlock

    while (data(pos) != '{' && pos < dataLen)
      pos += 1
    if (pos == dataLen) {
      if (!getMore())
        None // end-of-input
      else
        nextObject()
    } else {
      var objEnd = findObjEnd()
      if (objEnd > 0) {
        val idx = tokenizer.tokenize(data.slice(pos, objEnd))
        pos = objEnd
        Some(idx)
      } else if (!getMore())
        throw new JsonParseException("Incomplete (open) object or list in streaming JSON.  Didn't get closing }.", pos, true)
      else
        nextObject()
    }
  }

  def getMore() = { // returns T/F depending on whether we successfully read more into buffer
    if (pos > 0)
      shiftBlock(pos)
    else
      throw JsonParseException(s"JSON streaming buffer block size ($BLOCK_SIZE) is too small for this data.  Try again with a bigger value.", pos)
    appendBlock
  }

  private def findObjEnd() = {
    var tp = pos
    var found = false
    var skip = 0
    var inString = false
    var objStack = 0
    while (!found) {
      data(tp) match {
        case '{' if (!inString) => objStack += 1
        case '}' if (!inString) =>
          objStack -= 1
          if (objStack == 0) {
            //tp -= 1
            found = true
          }
        case '"' if (skip == 0) => inString = !inString
        case '\\'               => skip = 2
        case _                  =>
      }
      if (skip > 0) skip -= 1
      tp += 1
      if (tp == dataLen) {
        tp = -1
        found = true
      }
    }
    tp
  }

  private def appendBlock = {
    val readCount = in.read(data, dataLen, BLOCK_SIZE)
    if (readCount < 0)
      false
    else {
      dataLen += readCount
      true
    }
  }

  private def shiftBlock(startPos: Int) = {
    var j = 0
    for (i <- startPos to dataLen - 1) {
      data(j) = data(i)
      j += 1
    }
    dataLen -= startPos
    pos = 0
  }

  // def show(z:Option[IndexBuffer]) = {
  // 	val prefix = s"($dataLen): "
  // 	val content = new String(data.slice(0,dataLen))
  // 	println(s"$prefix|$content|")
  // 	if( z.isDefined ) {
  // 		val elements = z.get.position.slice(0,z.get.count)
  // 		print(" "*(prefix.length+1))
  // 		(0 to content.length).foreach( i => if(elements.contains(i)) print("+") else print("-")) 
  // 		println("")
  // 	}
  // }
}
