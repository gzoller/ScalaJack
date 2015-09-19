package co.blocke.scalajack
package json

case class JsonIndex(
	tokCount  : Int,          // how many token elements in the following arrays
	tokPos    : Array[Int],   // starting positions of each token
	tokLen    : Array[Int],   // length of each token
	tokType   : Array[Byte],  // token type (see JsonTokens for possible values)
	data      : Array[Char]   // the actual JSON
) {
	override def toString() = 
		s"Indexes(${tokPos.slice(0,tokCount).toList},${tokLen.slice(0,tokCount).toList},${tokType.slice(0,tokCount).toList.map(JsonTokens.toName(_))})"

	// Extract a token value at index i from the given StringBuilder
	def getToken(i:Int) = new String(data.slice(tokPos(i), tokPos(i)+tokLen(i)))

	import JsonTokens._
	def toCollection() : Either[Map[String,_],List[_]] =
		tokType(0) match {
			case JSobjStart  => Left(goDeepMap(1)._1)
			case JSlistStart => Right(goDeepList(1)._1)
			case x => println("Boom: "+x)
			Right(List.empty[String])
		}

	private def goDeepMap(startI:Int)  : (Map[String,Any],Int) = {
		var m = Map.empty[String,Any]
		var i = startI
		while( tokType(i) != JSobjEnd && tokType(i) != JSobjEndInList ) {
			val key = getToken(i)
			i += 1
			val (value,j) = goDeepValue(i)
			i = j + 1
			m = m + (key->value)
		}
		(m,i)
	}
	private def goDeepList(startI:Int) : (List[Any],Int) = {
		var l = List.empty[Any]
		var i = startI
		while( tokType(i) != JSlistEnd && tokType(i) != JSlistEndInList ) {
			val (value,j) = goDeepValue(i)
			l = l :+ value
			i = j + 1
		}
		(l,i)
	}
	private def goDeepValue(startI:Int) : (Any,Int) = {
		var i = startI
		tokType(i) match {
			case JSobjStart => 
				goDeepMap(i+1)
			case JSlistStart => 
				goDeepList(i+1)
			case JStrue | JStrueInList =>
				(true,i)
			case JSfalse | JSfalseInList =>
				(false,i)
			case JSnull | JSnullInList =>
				(null,i)
			case JSstring | JSstringInList =>
				(getToken(i),i)
			case JSnumber | JSnumberInList =>
				val num = getToken(i)
				if( num.contains('.') )
					(num.toDouble,i)
				else
					(num.toLong,i)
			case x => println("BOOM: "+x)
			(null,i)
		}
	}
}

/*
	val JSobjStart      : Byte = 1
	val JSobjEnd        : Byte = 15
	val JSobjEndInList  : Byte = 30  // JSobjEnd << 1
	val JSobjEndObjKey  : Byte = 60  // JSobjEnd << 2  *WARNING* Non-standard JSON
	val JSlistStart     : Byte = 26
	val JSlistEnd       : Byte = 7
	val JSlistEndInList : Byte = 14  // JSlistEnd << 1
	val JSlistEndObjKey : Byte = 28  // JSlistEnd << 2  *WARNING* Non-standard JSON
	val JStrue          : Byte = 8
	val JStrueInList    : Byte = 16  // JStrue << 1
	val JSfalse         : Byte = 9
	val JSfalseInList   : Byte = 18  // JSfalse << 1
	val JSnull          : Byte = 10
	val JSnullInList    : Byte = 20  // JSnull << 1
	val JSstring        : Byte = 11
	val JSstringInList  : Byte = 22  // JSstring << 1
	val JSstringObjKey  : Byte = 44  // JSstring << 2
	val JSnumber        : Byte = 12
	val JSnumberInList  : Byte = 24  // JSnumber << 1
	val JSnumberObjKey  : Byte = 48  // JSnumber << 2  *WARNING* Non-standard JSON
*/