package co.blocke.scalajack
package parser

trait Path {
  def consume[IN](t:Token): Either[Path,IN]
  val acceptsKeyValuePairs: Boolean
}

trait TokenGenerator {
  def nextToken(isKeyValuePair: Boolean): Token
}

trait Parser {

  def parse[IN]( tokenGenerator: TokenGenerator, path: Path ): IN = {
    var ppath = path
    while(true) {
      val token = tokenGenerator.nextToken( ppath.acceptsKeyValuePairs )
      ppath.consume(token) match {
        case Right(t: IN) => return t
        case Left(p: Path) => ppath = p
      }
    }
    ??? // Never gets here!
  }

}
