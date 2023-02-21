#######################################
#
# Construct adapters
#
# Each adapter handles (de)serialization of variable length integer
#######################################

import construct as con
from construct import (
    byte2int, int2byte, stream_read, stream_write, Construct, singleton, IntegerError, integertypes
)


@singleton
class IdaVarInt16(Construct):
    r"""
    construct adapter that handles (de)serialization of variable length int16 (see pack_dw/unpack_dw in IDA API)
    """

    def _parse(self, stream, context, path):
        b = byte2int(stream_read(stream, 1, path))
        extrabytes, mask = [
            # lookup table
            [0, 0xff], # (0b0xxxxxxx)
            [0, 0xff], # (0b0xxxxxxx)
            [1, 0x7f], # 0x80 (0b10xxxxxx)
            [2, 0x00]  # 0xC0 (0b11xxxxxx)
        ][b >> 6]

        num = b & mask
        for _ in range(extrabytes):
            num = (num << 8) + byte2int(stream_read(stream, 1, path))

        return num

    def _build(self, obj, stream, context, path):
        if not isinstance(obj, integertypes):
            raise IntegerError("value is not an integer", path=path)
        if obj < 0:
            raise IntegerError("cannot build from negative number: %r" % (obj,), path=path)
        if obj > 0xFFFF:
            raise IntegerError("cannot build from number above short range: %r" % (obj,), path=path)

        x = obj

        if (x > 0x3FFF):
            x |= 0xFF0000
            nbytes = 3
        elif (x > 0x7F):
            x |= 0x8000
            nbytes = 2
        else:
            nbytes = 1

        for i in range(nbytes, 0, -1):
            stream_write(stream, int2byte((x >> (8*(i-1))) & 0xFF), 1, path)

        return obj

@singleton
class IdaVarInt32(Construct):
    r"""
    construct adapter that handles (de)serialization of variable length int32 (see pack_dd/unpack_dd in IDA API)
    """

    def _parse(self, stream, context, path):
        b = byte2int(stream_read(stream, 1, path))
        extrabytes, mask = [
            [0, 0xff], [0, 0xff], [0, 0xff], [0, 0xff], # (0b0..xxxxx)
            [1, 0x7f], [1, 0x7f], # 0x80 (0b10.xxxxx)
            [3, 0x3f], # 0xC0 (0b110xxxxx)
            [4, 0x00]  # 0xE0 (0b111xxxxx)
        ][b>>5]

        num = b & mask
        for _ in range(extrabytes):
            num = (num << 8) + byte2int(stream_read(stream, 1, path))

        return num


    def _build(self, obj, stream, context, path):
        if not isinstance(obj, integertypes):
            raise IntegerError("value is not an integer", path=path)
        if obj < 0:
            raise IntegerError("cannot build from negative number: %r" % (obj,), path=path)
        if obj > 0xFFFFFFFF:
            raise IntegerError("cannot build from number above integer range: %r" % (obj,), path=path)
        x = obj

        if (x > 0x1FFFFFFF):
            x |= 0xFF00000000
            nbytes = 5
        elif (x > 0x3FFF):
            x |= 0xC0000000
            nbytes = 4
        elif (x > 0x7F):
            x |= 0x8000
            nbytes = 2
        else:
            nbytes = 1

        for i in range(nbytes, 0, -1):
            stream_write(stream, int2byte((x >> (8*(i-1))) & 0xFF), 1, path)

        return obj

@singleton
class IdaVarInt64(Construct):
    """
    construct adapter that handles (de)serialization of variable length int64 (see pack_dq/unpack_dq in IDA API)
    """

    def _parse(self, stream, context, path):
        low = IdaVarInt32._parse(stream, context, path)
        high = IdaVarInt32._parse(stream, context, path)
        num = (high << 32) | low
        return num

    def _build(self, obj, stream, context, path):
        if not isinstance(obj, integertypes):
            raise IntegerError("value is not an integer", path=path)
        if obj < 0:
            raise IntegerError("cannot build from negative number: %r" % (obj,), path=path)
        if obj > 0xFFFFFFFFFFFFFFFF:
            raise IntegerError("cannot build from number above short range: %r" % (obj,), path=path)

        low = obj & 0xFFFFFFFF
        IdaVarInt32._build(low, stream, context, path)
        high = obj >> 32
        IdaVarInt32._build(high, stream, context, path)

        return obj




#######################################
#
# Basic types & helpers
#
#######################################

# String prefixed with a variable int size
VarString = con.PascalString(IdaVarInt32, "utf8")
# Bytes buffer prefixed with a variable int size
VarBuff = con.Prefixed(IdaVarInt32, con.GreedyBytes)

# "template" for defining object list, prefixed with a variable int size
def ObjectList(obj):
    return con.PrefixedArray(IdaVarInt32, obj)

