package co.blocke.scalajack
package json

case class JsonIndex(
	tokCount  : Int,          // how many token elements in the following arrays
	tokPos    : Array[Int],   // starting positions of each token
	tokLen    : Array[Int],   // length of each token
	tokType   : Array[Byte]   // token type (see JsonTokens for possible values)
) {
	override def toString() = 
		s"Indexes(${tokPos.slice(0,tokCount).toList},${tokLen.slice(0,tokCount).toList},${tokType.slice(0,tokCount).toList.map(JsonTokens.toName(_))})"

	// Extract a token value at index i from the given StringBuilder
	def getToken(i:Int, s:Array[Char]) = s.slice(tokPos(i), tokPos(i)+tokLen(i))
}
