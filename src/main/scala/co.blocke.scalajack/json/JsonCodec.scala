package co.blocke.scalajack
package json

import writing.*

trait JsonCodec[A] {
 
    // TBD... when we're ready to tackle reading!
    // def decodeValue(in: JsonReader, default: A): A = ${
    //     if (cfg.encodingOnly) '{ ??? }
    //     else genReadVal(rootTpe :: Nil, 'default, cfg.isStringified, false, 'in)
    // }

    def encodeValue(in: A, out: JsonOutput): Unit
}