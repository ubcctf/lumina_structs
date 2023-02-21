import construct as con
from .basetypes import *
import copy
from .tinfo import *

#TODO make this generic
class _SignedVarInt64Adapter(con.Adapter):
    def _decode(self, obj, context, path):
        res = obj - 1
        if res >= (1 << 63):
            res -= (1 << 64)
        return res

    def _encode(self, obj, context, path):
        obj = (obj + 1) & ((1 << 64) - 1)
        return obj
SignedVarInt64 = _SignedVarInt64Adapter(IdaVarInt64)

MetadataType = con.Enum(IdaVarInt32,
    MD_TYPE_INFO = 0x01,
    MD_VD_ELAPSED = 0x02,
    MD_FUNC_CMT = 0x03,
    MD_FUNC_REPCMT = 0x04,
    MD_INSN_CMT = 0x05,
    MD_INSN_REPCMT = 0x06,
    MD_EXTRA_CMT = 0x07,
    MD_USER_STKPNTS = 0x08,
    MD_FRAME_DESC = 0x09,
    MD_INSN_OPREPRS = 0x0a,
)

SerializedTypeInfo = con.NullTerminated(con.GreedyBytes, require=False)

Metadata_TypeInfo = con.Struct(
    "is_auto" / con.Byte,
    "tinfo" / SerializedTypeInfo,
    "names" / con.GreedyRange(TypeInfoString),
)

Metadata_VDElapsed = con.Struct(   #still no idea what this does yet
    "unk" / IdaVarInt64,
)

Metadata_FuncCmt = con.Struct(
    "text" / con.GreedyString("utf8"),
)

InsnCmt = con.Struct(
    "offset" / IdaVarInt32,
    "text" / con.PascalString(IdaVarInt32, "utf8"),
)

ExtraCmt = con.Struct(
    "offset" / IdaVarInt32,
    "anterior" / con.PascalString(IdaVarInt32, "utf8"),
    "posterior" / con.PascalString(IdaVarInt32, "utf8"),
)

# stkpnt_t
StackPnt = con.Struct(
    "offset" / IdaVarInt32,
    "spd" / SignedVarInt64,
)

class InsnAnnotations(con.Subconstruct):
    def _parse(self, stream, context, path):
        obj = con.ListContainer()
        offset = IdaVarInt32._parsereport(stream, context, path)
        did_reset = True
        try:
            while 1:
                fallback = stream.tell()
                offset_diff = con.Peek(IdaVarInt32)._parsereport(stream, context, path)
                if did_reset or offset_diff:
                    offset += offset_diff
                    sobj = self.subcon._parsereport(stream, context, path)
                    assert "offset" in sobj, "subconstruct must have offset field as the first element"
                    sobj["offset"] = offset
                    obj.append(sobj)
                    did_reset = False
                else:
                    _ = IdaVarInt32._parsereport(stream, context, path)
                    offset = IdaVarInt32._parsereport(stream, context, path)
                    did_reset = True
        except con.StopFieldError:
            pass
        except Exception:
            stream.seek(fallback, 0)

        return obj

    def _build(self, obj, stream, context, path):
        IdaVarInt32._build(0, stream, context, path)

        if not obj:
            return

        obj_it = iter(obj)
        first = next(obj_it)

        cur_offset = first["offset"]
        self.subcon._build(first, stream, context, path)

        for sobj in obj_it:
            offset = sobj["offset"]
            if offset <= cur_offset:
                IdaVarInt32._build(0, stream, context, path) # reset marker
                IdaVarInt32._build(0, stream, context, path) # new offset
                self.subcon._build(sobj, stream, context, path)
            else:
                sobj_copy = copy.copy(sobj)
                sobj_copy["offset"] = offset - cur_offset
                self.subcon._build(sobj_copy, stream, context, path)
            cur_offset = offset

Metadata_InsnCmt = InsnAnnotations(InsnCmt)
Metadata_ExtraCmt = InsnAnnotations(ExtraCmt)
Metadata_StackPnts = InsnAnnotations(StackPnt)
# refinfo_t
RefInfo = con.Struct(
    "target" / SignedVarInt64,
    "base" / SignedVarInt64,
    "tdelta" / SignedVarInt64,
    "flags" / IdaVarInt32, # REF_OFF64, etc.
)
FrameVarRepr = con.Struct(
    "flags" / con.Byte,
    "off_info" / con.If((con.this.flags & 0xf) == 0x5, RefInfo),
)
InsnOprepr = con.Struct(
    "offset" / IdaVarInt32,      #offset of insn
    "flags" / con.Byte,
    "off1_info" / con.If((con.this.flags & 0xf) == 0x5, RefInfo),   #first operand offset info if any else none
    "off2_info" / con.If((con.this.flags & 0xf0) == 0x50, RefInfo), #second operand
)
FrameVarType = con.Struct(
    "tinfo" / SerializedTypeInfo,
    "names" / con.NullTerminated(con.GreedyRange(TypeInfoString)),
)
FrameVar = con.Struct(
    "flags" / con.Byte,
    "name" / con.If(con.this.flags & 1, con.CString("utf8")),
    "type" / con.If(con.this.flags & 2, FrameVarType),
    "cmt" / con.If(con.this.flags & 4, con.CString("utf8")),
    "repcmt" / con.If(con.this.flags & 8, con.CString("utf8")),
    "off" / con.If(con.this.flags & 0x10, SignedVarInt64),
    "repr" / con.If(con.this.flags & 0x20, FrameVarRepr),
    "nbytes" / con.If(con.this.flags & 0x40, SignedVarInt64),
)
Metadata_FrameDesc = con.Struct(
    "frsize" / SignedVarInt64,
    "argsize" / SignedVarInt64, # purge
    "frregs" / IdaVarInt16,
    "vars" / ObjectList(FrameVar),
)
Metadata_InsnOpreprs = InsnAnnotations(InsnOprepr)

Metadata = con.Switch(con.this.type, {
    MetadataType.MD_TYPE_INFO: Metadata_TypeInfo,
    MetadataType.MD_VD_ELAPSED: Metadata_VDElapsed,
    MetadataType.MD_FUNC_CMT: Metadata_FuncCmt,
    MetadataType.MD_FUNC_REPCMT: Metadata_FuncCmt,
    MetadataType.MD_INSN_CMT: Metadata_InsnCmt,
    MetadataType.MD_INSN_REPCMT: Metadata_InsnCmt,
    MetadataType.MD_EXTRA_CMT: Metadata_ExtraCmt,
    MetadataType.MD_USER_STKPNTS: Metadata_StackPnts,
    MetadataType.MD_FRAME_DESC: Metadata_FrameDesc,
    MetadataType.MD_INSN_OPREPRS: Metadata_InsnOpreprs,
}, default = None)


MetadataChunk = con.Struct(
    "type" / MetadataType,
    "data" / con.Prefixed(IdaVarInt32, Metadata),
)

#requires seekable/tellable streams (rpc_message_parse has a special case for sockets that allow this already)
MetadataPayload = con.ExprAdapter( #rename for backwards compatibility
    con.RawCopy(con.Prefixed(IdaVarInt32, con.GreedyRange(MetadataChunk))),
    #container and dicts both support accessing with keys, while only container supports accessing by attr
    lambda c,_: con.Container(data=c['data'], chunks=c['value'], offset1=c['offset1'], offset2=c['offset2'], size=c['length']),
    #only value/data is consumed by RawCopy, so remap only chunks if exist
    lambda c,_: con.Container(**{'value' if k == 'chunks' else k:v for k,v in c.items()}))