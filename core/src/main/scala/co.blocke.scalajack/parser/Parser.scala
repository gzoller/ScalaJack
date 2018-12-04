package co.blocke.scalajack
package parser

trait Path[IN] {
  def consume(tGen: TokenGenerator): IN
}

trait TokenGenerator {
  def nextSimpleToken()                   : SimpleToken
  def nextArrayToken[T](elementPath: Path): Token
  def nextObjectToken(p: Path)            : Token
  def nextMapToken(p: Path)     : Token
}

trait TokenGeneratorFactory {
  def apply[S](source: S): TokenGenerator
}

trait Parser {

  def parse[S,IN]( source: S )(implicit tokenFactory: TokenGeneratorFactory): IN =
    parse( Path.Root, tokenFactory(source) )

  def parse[IN]( path: Path, tokenGenerator: TokenGenerator ): IN = path.consume(tokenGenerator)
//
//  {
//    var ppath = path
//    while(true) {
//      val token = tokenGenerator.nextToken( ppath.acceptsKeyValuePairs )
//      ppath.consume(token) match {
//        case Right(t: IN) => return t
//        case Left(p: Path) => ppath = p
//      }
//    }
//    ??? // Never gets here!
//  }

}
