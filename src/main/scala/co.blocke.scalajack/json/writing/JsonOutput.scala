package co.blocke.scalajack
package json
package writing

case class JsonOutput():
    val internal: StringBuilder = new StringBuilder()

    private var comma: Boolean = false

    def result = internal.result

    inline def startObject() = 
        internal.append('{')
        comma = false

    inline def endObject() = 
        internal.append('}')
        this

    inline def startArray() = 
        internal.append('[')
        comma = false

    inline def endArray() = 
        internal.append(']')
        this

    inline def maybeComma() = 
        if comma then 
        internal.append(',')
        comma = false

    inline def burpNull() = 
        internal.append("null")
        this

    inline def label( s: String ) = 
        maybeComma()
        internal.append("\""+s+"\":")

    inline def label( s: Long ) = 
        maybeComma()
        internal.append("\""+s+"\":")
    
    //----------------------- Primitive/Simple type support

    // TODO: BigDecimal, BigInt and Java equiv.

    inline def value(v: Boolean) =
        internal.append(v)
        comma = true
        this

    inline def value(v: Byte) =
        internal.append(v)
        comma = true
        this

    inline def value(v: Char) =
        internal.append("\""+v+"\"")
        comma = true
        this

    inline def value(v: Double) =
        internal.append(v)
        comma = true
        this

    inline def value(v: Float) =
        internal.append(v)
        comma = true
        this

    inline def value(v: Int) =
        internal.append(v)
        comma = true
        this

    inline def value(v: Long) =
        internal.append(v)
        comma = true
        this

    inline def value(v: Short) =
        internal.append(v)
        comma = true
        this

    inline def value(v: String) =
        if v == null then internal.append("null")
        else internal.append("\""+v+"\"")
        comma = true
        this

    inline def value(v: java.lang.Boolean) =
        if v == null then internal.append("null")
        else internal.append(v)
        comma = true
        this

    inline def value(v: java.lang.Byte) =
        if v == null then internal.append("null")
        else internal.append(v)
        comma = true
        this

    inline def value(v: java.lang.Character) =
        if v == null then internal.append("null")
        else internal.append("\""+v+"\"")
        comma = true
        this

    inline def value(v: java.lang.Double) =
        if v == null then internal.append("null")
        else internal.append(v)
        comma = true
        this

    inline def value(v: java.lang.Float) =
        if v == null then internal.append("null")
        else internal.append(v)
        comma = true
        this

    inline def value(v: java.lang.Integer) =
        if v == null then internal.append("null")
        else internal.append(v)
        comma = true
        this

    inline def value(v: java.lang.Long) =
        if v == null then internal.append("null")
        else internal.append(v)
        comma = true
        this

    inline def value(v: java.lang.Short) =
        if v == null then internal.append("null")
        else internal.append(v)
        comma = true
        this

    inline def value(v: java.lang.Number) =
        if v == null then internal.append("null")
        else internal.append(v)
        comma = true
        this

    // TODO: UUID