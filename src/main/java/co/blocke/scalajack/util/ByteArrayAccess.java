package co.blocke.scalajack.util;

import java.lang.invoke.MethodHandles;
import java.lang.invoke.VarHandle;
import java.nio.ByteOrder;

public class ByteArrayAccess { // FIXME: Use Java wrapper as w/a for missing support of @PolymorphicSignature methods in Scala 3, see: https://github.com/lampepfl/dotty/issues/11332
    private static final VarHandle VH_LONG =
            MethodHandles.byteArrayViewVarHandle(long[].class, ByteOrder.LITTLE_ENDIAN);
    private static final VarHandle VH_INT =
            MethodHandles.byteArrayViewVarHandle(int[].class, ByteOrder.LITTLE_ENDIAN);
    private static final VarHandle VH_SHORT =
            MethodHandles.byteArrayViewVarHandle(short[].class, ByteOrder.LITTLE_ENDIAN);
    private static final VarHandle VH_LONG_REVERSED =
            MethodHandles.byteArrayViewVarHandle(long[].class, ByteOrder.BIG_ENDIAN);
    private static final VarHandle VH_INT_REVERSED =
            MethodHandles.byteArrayViewVarHandle(int[].class, ByteOrder.BIG_ENDIAN);

    static public void setLong(byte[] buf, int pos, long value) {
        VH_LONG.set(buf, pos, value);
    }

    static public long getLong(byte[] buf, int pos) {
        return (long) VH_LONG.get(buf, pos);
    }

    static public void setInt(byte[] buf, int pos, int value) {
        VH_INT.set(buf, pos, value);
    }

    static public int getInt(byte[] buf, int pos) {
        return (int) VH_INT.get(buf, pos);
    }

    static public void setShort(byte[] buf, int pos, short value) {
        VH_SHORT.set(buf, pos, value);
    }

    static public void setLongReversed(byte[] buf, int pos, long value) {
        VH_LONG_REVERSED.set(buf, pos, value);
    }

    static public int getIntReversed(byte[] buf, int pos) {
        return (int) VH_INT_REVERSED.get(buf, pos);
    }
}