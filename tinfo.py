from .basetypes import *
from .util import AttrDict
import math

@singleton
class TypeVarInt15(Construct):
    r"""
    construct adapter that handles (de)serialization of tinfo_t dt types  (see get_dt)
    """

    def _parse(self, stream, context, path):
        b = byte2int(stream_read(stream, 1, path))

        if b & 0b10000000:  #we are in 2 bytes range   
            b = (b & 0b01111111) + (stream_read(stream, 1, path)[0] << 7)
            #its ""somewhat"" little endian

        return b - 1

    #TODO test
    def _build(self, obj, stream, context, path):
        if not isinstance(obj, integertypes):
            raise IntegerError("value is not an integer", path=path)
        if obj < 0:
            raise IntegerError("cannot build from negative number: %r" % (obj,), path=path)
        if obj > 0x7FFE:
            raise IntegerError("cannot build from number above short range: %r" % (obj,), path=path)

        if obj > 0x7F:
            payload = bytes([obj / 0x80, obj % 0x80 + 1])
            stream_write(stream, payload, len(payload), path)

        return obj

@singleton
class TypeVarInt32(Construct):
    r"""
    construct adapter that handles (de)serialization of tinfo_t de types (see get_de)
    40bit base / 40bit num_elems
    """

    def _parse(self, stream, context, path):
        val = 0
        while True:
            curr = stream_read(stream, 1, path)[0]
            if curr & 0b10000000:
                val = val << 7 | curr & 0b01111111
            else:   #last byte has 0b01000000
                val = val << 6 | curr & 0b00111111
                break

        return val

    #TODO test
    def _build(self, obj, stream, context, path):
        if not isinstance(obj, integertypes):
            raise IntegerError("value is not an integer", path=path)
        if obj < 0:
            raise IntegerError("cannot build from negative number: %r" % (obj,), path=path)
        if obj > 0xFFFFFFFF:
            raise IntegerError("cannot build from number above 32 bit: %r" % (obj,), path=path)

        b = []
        cascade = False   #once the highest bit location has been found we dont care about whether the lower bits are zero or not - we always build
        if obj >> 27:
            b.append(((obj >> 27) & 0b01111111) | 0b10000000)
            cascade = True
        if cascade or obj >> 20:
            b.append(((obj >> 20) & 0b01111111) | 0b10000000)
            cascade = True
        if cascade or obj >> 13:
            b.append(((obj >> 13) & 0b01111111) | 0b10000000)
            cascade = True
        if cascade or obj >> 6:
            b.append(((obj >> 6) & 0b01111111) | 0b10000000)

        #always craft last byte
        b.append((obj & 0b00111111) | 0b01000000)

        stream_write(stream, bytes(b), len(b), path)
        return obj

@singleton
class TypeArrayData(Construct):
    r"""
    construct adapter that handles (de)serialization of tinfo_t da types (see get_da)
    40bit base / 40bit num_elems
    """

    def _parse(self, stream, context, path):
        data = stream_read(stream, 9, path)

        bb, bne = data[:4], data[5:]

        #similar to big endian?
        bv = 0
        for b in bb:
            bv = bv << 7 | (b & 0b01111111)

        nev = 0
        for b in bne:
            nev = nev << 7 | (b & 0b01111111)

        return con.Container(base=(bv << 4) | (data[4] & 0xF), num_elems=(data[4] >> 4) | nev)

    #TODO test
    def _build(self, obj, stream, context, path):
        if not isinstance(obj, integertypes):
            raise IntegerError("value is not an integer", path=path)
        if obj < 0:
            raise IntegerError("cannot build from negative number: %r" % (obj,), path=path)
        if obj > 0x7FFE:
            raise IntegerError("cannot build from number above 0x7FFE: %r" % (obj,), path=path)

        if type(obj) == dict:
            obj = AttrDict(obj)

        bv, nev = obj.base, obj.num_elems
        bb, bne = [], []

        while bv:
            bb.append(bv & 0b01111111)
            bv = bv >> 7

        while nev:
            bne.append(nev & 0b01111111)
            nev = nev >> 7

        stream_write(stream, bb[:4] + bytes([bb[5] & 0xF | bne << 4]) + bne[1:], 9, path)
        return obj


# ref p_string
TypeInfoString = con.PascalString(TypeVarInt15, "utf8")

RESERVED_BYTE = 0xFF   #multipurpose(?)


# Common Types


# IDs for common types
CommonTypes = con.Enum(con.Byte,
    STI_PCHAR = 0x0,           #< char *
    STI_PUCHAR = 0x1,          #< uint8 *
    STI_PCCHAR = 0x2,          #< const char *
    STI_PCUCHAR = 0x3,         #< const uint8 *
    STI_PBYTE = 0x4,           #< _BYTE *
    STI_PINT = 0x5,            #< int *
    STI_PUINT = 0x6,           #< unsigned int *
    STI_PVOID = 0x7,           #< void *
    STI_PPVOID = 0x8,          #< void **
    STI_PCVOID = 0x9,          #< const void *
    STI_ACHAR = 0xA,           #< char[]
    STI_AUCHAR = 0xB,          #< uint8[]
    STI_ACCHAR = 0xC,          #< const char[]
    STI_ACUCHAR = 0xD,         #< const uint8[]
    STI_FPURGING = 0xE,        #< void __userpurge(int)
    STI_FDELOP = 0xF,          #< void __cdecl(void *)
    STI_MSGSEND = 0x10,        #< void *(void *, const char *, ...)
    STI_AEABI_LCMP = 0x11,     #< int __fastcall(int64 x, int64 y)
    STI_AEABI_ULCMP = 0x12,    #< int __fastcall(uint64 x, uint64 y)
    STI_DONT_USE = 0x13,       #< unused stock type id; should not be used
    STI_SIZE_T = 0x14,         #< size_t
    STI_SSIZE_T = 0x15,        #< ssize_t
    STI_AEABI_MEMCPY = 0x16,   #< void __fastcall(void *, const void *, size_t)
    STI_AEABI_MEMSET = 0x17,   #< void __fastcall(void *, size_t, int)
    STI_AEABI_MEMCLR = 0x18,   #< void __fastcall(void *, size_t)
    STI_RTC_CHECK_2 = 0x19,    #< int16 __fastcall(int16 x)
    STI_RTC_CHECK_4 = 0x1A,    #< int32 __fastcall(int32 x)
    STI_RTC_CHECK_8 = 0x1B,    #< int64 __fastcall(int64 x)
    STI_LAST = 0x1C,
)


# type_t Definitions


# ref tf_mask
TYPE_SIZE  = con.BitsInteger(4)   
TYPE_BASE_MASK  = 0x0F

FLAGS_SIZE = con.BitsInteger(2)   
TYPE_FLAGS_MASK = 0x30

MOD_SIZE   = con.BitsInteger(2)   
TYPE_MODIF_MASK = 0xC0


# ref tf
BaseTypes = con.Enum(TYPE_SIZE,
    BT_UNK         = 0x00,     #< unknown
    BT_VOID        = 0x01,     #< void
    BT_INT8        = 0x02,     #< __int8
    BT_INT16       = 0x03,     #< __int16
    BT_INT32       = 0x04,     #< __int32
    BT_INT64       = 0x05,     #< __int64
    BT_INT128      = 0x06,     #< __int128 (for alpha & future use)
    BT_INT         = 0x07,     #< natural int. (size provided by idp module)
    BT_BOOL        = 0x08,     #< bool
    BT_FLOAT       = 0x09,     #< float
    BT_PTR         = 0x0A,     #< pointer.
    BT_ARRAY       = 0x0B,     #< array
    BT_FUNC        = 0x0C,     #< function.
    BT_COMPLEX     = 0x0D,     #< struct/union/enum/typedef.
    BT_BITFIELD    = 0x0E,     #< bitfield (only in struct)
    BT_RESERVED    = 0x0F,     #< RESERVED
)
#BT_INT might be a bit of a pain to support across archs

# start basic type flags

# ref tf_unk
VoidFlags = con.Enum(FLAGS_SIZE,
    BTMT_SIZE0   = 0x0,        #< ::BT_VOID - normal void; ::BT_UNK - don't use
    BTMT_SIZE12  = 0x1,        #< size = 1  byte  if ::BT_VOID; 2 if ::BT_UNK
    BTMT_SIZE48  = 0x2,        #< size = 4  bytes if ::BT_VOID; 8 if ::BT_UNK
    BTMT_SIZE128 = 0x3,        #< size = 16 bytes if ::BT_VOID; unknown if ::BT_UNK
                               #< (IN struct alignment - see below)
)

# ref tf_int
IntFlags = con.Enum(FLAGS_SIZE,
    BTMT_UNKSIGN = 0x0,        #< unknown signedness
    BTMT_SIGNED  = 0x1,        #< signed
    BTMT_USIGNED = 0x2,        #< unsigned
    BTMT_CHAR    = 0x3,        #< specify char or segment register
                               #< - ::BT_INT8         - char
                               #< - ::BT_INT          - segment register
                               #< - other BT_INT...   - don't use
)

# ref tf_bool
BoolFlags = con.Enum(FLAGS_SIZE,
    BTMT_DEFBOOL = 0x0,        #< size is model specific or unknown(?)
    BTMT_BOOL1   = 0x1,        #< size 1byte
    BTMT_BOOL2OR8= 0x2,        #< size 2bytes - !inf_is_64bit(); size 8bytes - inf_is_64bit()
    BTMT_BOOL4   = 0x3,        #< size 4bytes
)
#pain another arch dependent flag

# ref tf_float
FloatFlags = con.Enum(FLAGS_SIZE,
    BTMT_FLOAT   = 0x0,        #< float (4 bytes)
    BTMT_DOUBLE  = 0x1,        #< double (8 bytes)
    BTMT_LNGDBL  = 0x2,        #< long double (compiler specific)
    BTMT_SPECFLT = 0x3,        #< float (variable size).
                               #< if \ph{use_tbyte()} then use \ph{tbyte_size},
                               #< otherwise 2 bytes
)

# end basic type flags (ref tf_last_basic)

# ref tf_ptr
PtrFlags = con.Enum(FLAGS_SIZE,
    BTMT_DEFPTR  = 0x0,        #< default for model
    BTMT_NEAR    = 0x1,        #< near
    BTMT_FAR     = 0x2,        #< far
    BTMT_CLOSURE = 0x3,        #< closure.
                               #< - if ptr to ::BT_FUNC - __closure.
                               #<   in this case next byte MUST be
                               #<   #RESERVED_BYTE, and after it ::BT_FUNC
                               #< - else the next byte contains sizeof(ptr)
                               #<   allowed values are 1 - \varmem{ph,processor_t,max_ptr_size}
                               #< - if value is bigger than \varmem{ph,processor_t,max_ptr_size},
                               #<   based_ptr_name_and_size() is called to
                               #<   find out the typeinfo
)

# ref tf_array
ArrayFlags = con.Enum(FLAGS_SIZE,
    BTMT_NONE      = 0x0,      #(non-IDA) signifies no flags for array
    BTMT_NONBASED  = 0x1,      #< \code
                               # if set
                               #    array base==0
                               #    format: dt num_elem; [tah-typeattrs]; type_t...
                               #    if num_elem==0 then the array size is unknown
                               # else
                               #    format: da num_elem, base; [tah-typeattrs]; type_t... \endcode
                               # used only for serialization
    BTMT_ARRESERV  = 0x2,      #< reserved bit
)

# ref tf_func
FuncFlags = con.Enum(FLAGS_SIZE,
    BTMT_DEFCALL  = 0x0,       #< call method - default for model or unknown
    BTMT_NEARCALL = 0x1,       #< function returns by retn
    BTMT_FARCALL  = 0x2,       #< function returns by retf
    BTMT_INTCALL  = 0x3,       #< function returns by iret
                               #< in this case cc MUST be 'unknown'
)

# ref tf_complex
ComplexFlags = con.Enum(FLAGS_SIZE,
    BTMT_STRUCT  = 0x0,        #<     struct:
                               #<       MCNT records: type_t; [sdacl-typeattrs];
    BTMT_UNION   = 0x1,        #<     union:
                               #<       MCNT records: type_t...
    BTMT_ENUM    = 0x2,        #<     enum:
                               #<       next byte bte_t (see below)
                               #<       N records: de delta(s)
                               #<                  OR
                               #<                  blocks (see below)
    BTMT_TYPEDEF = 0x3,        #< named reference
                               #<   always p_string name
)

BitFieldFlags = con.Enum(FLAGS_SIZE,
    BTMT_BFLDI8    = 0x0,      #< __int8
    BTMT_BFLDI16   = 0x1,      #< __int16
    BTMT_BFLDI32   = 0x2,      #< __int32
    BTMT_BFLDI64   = 0x3,      #< __int64
)

# end flags

FlagsMapping = con.Switch(lambda this: this.basetype, {
    BaseTypes.BT_VOID    : VoidFlags,
    BaseTypes.BT_INT8    : IntFlags,
    BaseTypes.BT_INT16   : IntFlags,
    BaseTypes.BT_INT32   : IntFlags,
    BaseTypes.BT_INT64   : IntFlags,
    BaseTypes.BT_INT128  : IntFlags,
    BaseTypes.BT_INT     : IntFlags,
    BaseTypes.BT_BOOL    : BoolFlags,
    BaseTypes.BT_FLOAT   : FloatFlags,
    BaseTypes.BT_PTR     : PtrFlags,
    BaseTypes.BT_ARRAY   : ArrayFlags,
    BaseTypes.BT_FUNC    : FuncFlags,
    BaseTypes.BT_COMPLEX : ComplexFlags,
    BaseTypes.BT_BITFIELD: BitFieldFlags,
}, default=FLAGS_SIZE)   #leave it as bits


# ref tf_modifiers
# mutually exclusive; only applies for BT_ARRAY and BT_VOID?
Modifiers = con.Enum(MOD_SIZE,
    BTM_NONE = 0x0,          #(non-IDA) signifies no modifiers
    BTM_CONST = 0x1,         #< const
    BTM_VOLATILE = 0x2,      #< volatile
)

#typedef uchar type_t - 4/2/2 bits, see ref tf
type_t = con.Restreamed(con.Struct(
    "basetype" / BaseTypes,
    "flags" / FlagsMapping,
    "modifiers" / Modifiers,
), lambda data: (s:=con.bytes2bits(data))[4:]+s[2:4]+s[:2], 1
, lambda data: con.bits2bytes(data[:2]+data[2:4]+data[:4]), 8, 1)  #we know type_t is 1 byte, so no need for a lambda
#reorders the bitfields so FlagsMapping can parse correctly

# Enum flags definitions
OutputStyle = con.Enum(con.BitsInteger(2),
    BTE_HEX  = 0b00,        #< hex
    BTE_CHAR = 0b01,        #< char or hex
    BTE_SDEC = 0b10,        #< signed decimal
    BTE_UDEC = 0b11,        #< unsigned decimal
)

bte_t = con.BitStruct(
    "always" / con.ExprValidator(con.Flag, lambda obj,_: obj),        #< this bit MUST be present (failfast)
    "style" / OutputStyle,                                            #< output style mask
    "bitfield" / con.Flag,                                            #< 'subarrays'. In this case ANY record
                                                                      #< has the following format:
                                                                      #<   - 'de' mask (has name)
                                                                      #<   - 'dt' cnt
                                                                      #<   - cnt records of 'de' values
                                                                      #<      (cnt CAN be 0)
                                                                      #< \note delta for ALL subsegment is ONE
    "reserved" / con.ExprValidator(con.Flag, lambda obj,_: not obj),  #< must be 0, in order to distinguish
                                                                      #< from a tah-byte
    "size" / con.ExprAdapter(con.BitsInteger(3),                      #< storage size.
                                lambda n,_: 1 << (n-1) if n else -1,  #<   - if == 0 then inf_get_cc_size_e()            
                                lambda n,_: int(math.log(n,2)) + 1),  #<   - else 1 << (n -1) = 1,2,4...64
                                #-1 sigifies default (cc_size_e)
)


# TAH (type attribute header) definitions


TAFLAGS_SIZE = con.BytesInteger(2)         #type attribute flags size (ref IDA 7.5)

TAH_BYTE = 0xFE                #< type attribute header byte
FAH_BYTE = 0xFF                #< function argument attribute header byte

TAH_HASATTRS = 0x0010          #< has extended attributes

# ref tattr_udt (Type attributes for udts)  (udt = struct || union, complex type)
UdtAttrFlags = con.FlagsEnum(TAFLAGS_SIZE,
    TAUDT_UNALIGNED = 0x0040,  #< struct: unaligned struct
    TAUDT_MSSTRUCT  = 0x0020,  #< struct: gcc msstruct attribute
    TAUDT_CPPOBJ    = 0x0080,  #< struct: a c++ object, not simple pod type
    TAUDT_VFTABLE   = 0x0100,  #< struct: is virtual function table
)

# ref tattr_field (Type attributes for udt fields)
UdtFieldAttrFlags = con.FlagsEnum(TAFLAGS_SIZE,
    TAFLD_BASECLASS = 0x0020,  #< field: do not include but inherit from the current field
    TAFLD_UNALIGNED = 0x0040,  #< field: unaligned field
    TAFLD_VIRTBASE  = 0x0080,  #< field: virtual base (not supported yet)
    TAFLD_VFTABLE   = 0x0100,  #< field: ptr to virtual function table
)

# ref tattr_ptr (Type attributes for pointers)
PtrAttrFlags = con.FlagsEnum(TAFLAGS_SIZE,
    TAPTR_PTR32     = 0x0020,  #< ptr: __ptr32
    TAPTR_PTR64     = 0x0040,  #< ptr: __ptr64
    TAPTR_RESTRICT  = 0x0060,  #< ptr: __restrict
    TAPTR_SHIFTED   = 0x0080,  #< ptr: __shifted(parent_struct, delta)
)

# ref tattr_enum (Type attributes for enums)
EnumAttrFlags = con.FlagsEnum(TAFLAGS_SIZE,
    TAENUM_64BIT    = 0x0020,  #< enum: store 64-bit values
    TAENUM_UNSIGNED = 0x0040,  #< enum: unsigned
    TAENUM_SIGNED   = 0x0080,  #< enum: signed
)

#i believe these are the only ones that are implemented right now, but basically all types' format aside from bitfields have optional tah-typeattrs fields
AttrMapping = con.Switch(lambda this: this.type, {
    BaseTypes.BT_PTR      : PtrAttrFlags,
    BaseTypes.BT_COMPLEX  : UdtAttrFlags,        #expects BT_COMPLEX to mean struct | union only
    ComplexFlags.BTMT_ENUM: EnumAttrFlags, 
    None                  : UdtFieldAttrFlags,   #None signifies any types that are fields of a udt type (currently unused by this parser)
}, default=TAFLAGS_SIZE)


@singleton
class TypeInfo(Construct):
    r"""
    construct adapter that handles (de)serialization of tinfo_t
    """

    #sdacl's check routine should be exactly the same as tah
    #it's very likely that cross-disasm support can be implemented without the full feature set like tah/sdacl so the build counterpart of this is probably not gonna be implemented
    #but if it will be, then this method should move to a separate construct class
    #TODO test sdacl and hasattrs (all these code is from translating pure reversed code so take it with a huge grain of salt lmao)
    def check_tah_or_sdacl(self, stream, type, path, sdacl = False):
        byte = stream.read(1)   #can't peek with all streams, implement it the dumb way instead
        if byte:   #do something only when we read something
            #sdacl can clash with other bytes - only parse as sdacl when we expect it (aka sdacl is True)
            if byte[0] == TAH_BYTE or (sdacl and (((byte[0] & ~TYPE_FLAGS_MASK) ^ TYPE_MODIF_MASK) <= int(BaseTypes.BT_VOID))):   #is TAH or SDACL headers
                sdacl_variable_bits = (byte[0] & 1 | (byte[0] >> 3) & 6)

                val = 0
                if byte[0] == TAH_BYTE or sdacl_variable_bits == 0b111:
                    bit = 0
                    #assumes this is a valid serialization of tah bytes, otherwise probably returns garbage (checks IDA implemented is not implemented here)
                    while True:    #read it similar to da, but little endian and with variable size
                        b = stream_read(stream, 1, path)[0]
                        val |= (b & 0b01111111) << bit
                        if b & 0b10000000 == 0:
                            break
                        bit += 7
                else:  #not new headers, handle it the legacy way(?) and treat the variable bits as the value directly
                    val = sdacl_variable_bits + 1

                attrs = []
                if val & TAH_HASATTRS:  #has attributes, otherwise only has flags
                    n = TypeVarInt15.parse_stream(stream)
                    
                    for _ in range(n):
                        name = TypeInfoString.parse_stream(stream)
                        #TODO logic on 0xAE/0xAC/0xFD checking? probably not
                        #TODO figure out how this maps to the AttrMapping flags
                        data = stream_read(stream, TypeVarInt15.parse_stream(stream), path)
                        attrs.append(con.Container(name=name, data=data))

                return con.Container(flags=AttrMapping.parse(val.to_bytes(TAFLAGS_SIZE.sizeof(), byteorder='big'), type=type), attrs=con.ListContainer(attrs))
            else:
                #likely not TAH or SDACL, reset file pointer and leave
                stream.seek(stream.tell()-1)
        return None

    def _parse(self, stream, context, path):
        typedef = type_t.parse_stream(stream)

        data, rest, tah = None, b'', None
        if int(typedef.basetype) > int(BaseTypes.BT_FLOAT):  #only process advanced types; basic types have no special logic aside from optional tah
            if typedef.basetype == BaseTypes.BT_PTR:
                #TODO it is db ([db sizeof(ptr)]; [tah-typeattrs]; type_t...)
                #TODO test closure somehow
                size = stream_read(stream, 1, path)  #even if we dont have size we must still have at least 1 byte left
                if typedef.flags != PtrFlags.BTMT_CLOSURE: #if not closure theres no ptr size
                    stream.seek(stream.tell()-1)
                    size = None

                tah = self.check_tah_or_sdacl(stream, typedef.basetype, path)

                #if closure and size == RESERVED_BYTE, implicitly expects the parsed tinfo is of BT_FUNC, and size is ignored
                data = con.Container(ptrsize=size, type=TypeInfo.parse_stream(stream))
                
            elif typedef.basetype == BaseTypes.BT_ARRAY:
                if typedef.flags == ArrayFlags.BTMT_NONBASED:
                    base = 0
                    size = TypeVarInt15.parse_stream(stream)
                else:
                    base, size = TypeArrayData.parse_stream(stream).values()
                
                tah = self.check_tah_or_sdacl(stream, typedef.basetype, path)
                
                data = con.Container(base=base, num_elems=size, type=TypeInfo.parse_stream(stream))

            elif typedef.basetype == BaseTypes.BT_FUNC:
                #TODO impl
                rest += con.GreedyBytes.parse_stream(stream)
                pass

            elif typedef.basetype == BaseTypes.BT_COMPLEX:
                if typedef.flags == ComplexFlags.BTMT_TYPEDEF:
                    #only useful complex type wrt Lumina - see comment below
                    data = con.Container(name=TypeInfoString.parse_stream(stream))
                else:
                    #IDA does not push typerefs verbatim - instead it represents it as a typedef with the ordinal string ('#' + set_de(ord)) as name
                    #Lumina (calc_func_metadata) always call replace_ordinal_typerefs before serialize_tinfo, so instead of the ordinal string it is replaced with the actual type name
                    #to get the actual serialized type definition from the ordinal, something like get_numbered_type will have to be called, but they are never called in Lumina afaict
                    #(which checks out since Lumina does not have any way to pass things like field names, but this also means that complex types never gets pushed unfortunately)
                    #the best thing one can do when applying the info obtained from Lumina is probably to check whether the type name exists in the disasm database, and utilize it if the size matches nbytes; otherwise probably make an empty struct of size nbytes and let ppl fill it in later
                    #and the actual struct definitions themselves will likely have to come from exporting local types as a header file and parsing it in the other disassemblers manually

                    n = TypeVarInt15.parse_stream(stream)
                    if n == 0:  #treat the same as a typedef if there are no member fields
                        data = con.Container(name=TypeInfoString.parse_stream(stream))
                        tah = self.check_tah_or_sdacl(stream, typedef.basetype, path, True)  #sdacl
                    else:
                        if n == 0x7FFE:   #support high member count; this is technically defined separately for enum and struct & union, but should work the same here
                            n = TypeVarInt32.parse_stream(stream)  #discard existing n from dt (idk why IDA chose such a wasteful method - i wouldve thought they would reuse the 0x7FFE value)

                        #sdacl for structs & unions; tah for enums
                        #(technically defined separately again at the end of each respective case, but IDA does it at the same time internally just like we are doing right now)
                        tah = self.check_tah_or_sdacl(stream, typedef.basetype if typedef.flags != ComplexFlags.BTMT_ENUM else ComplexFlags.BTMT_ENUM, path, typedef.flags != ComplexFlags.BTMT_ENUM)

                        if typedef.flags == ComplexFlags.BTMT_ENUM:
                            #bte_t; list of de delta(s) OR blocks(bitfield?) of size n

                            bte = bte_t.parse_stream(stream)

                            if bte.bitfield:  #blocks (defined in BTE_BITFIELD)
                                #TODO somehow test - it seems like IDA never carries the bitfield definitions forward from IDB format to TIL format so this never gets set
                                vals = []
                                for _ in range(n):
                                    mask = TypeVarInt32.parse_stream(stream)
                                    cnt = TypeVarInt15.parse_stream(stream)
                                    subarr = con.ListContainer([TypeVarInt32.parse_stream(stream) for _ in range(cnt)])
                                    vals.append(con.Container(mask=mask, cnt=cnt, subarr=subarr))  #subarr probably also have delta values
                                data = con.Container(flags=bte, vals=con.ListContainer(vals))
                            else:  #much easier: de delta(s) (delta is difference between each enum value)
                                #TODO signedness (make TypeVarInt32 extend BytesInteger?)
                                #delta can be negative (currently represented as unsigned 32bit value) if width is smaller than required to fit all values
                                #TAENUM_64BIT is supported by mashing 2 de types (32bit each) into one value; width can be bigger than 8 bytes (64 bit), but values get truncated to 8 bytes anyway
                                if tah and tah.flags.TAENUM_64BIT:
                                    deltas = [TypeVarInt32.parse_stream(stream) | TypeVarInt32.parse_stream(stream) << 32 for _ in range(n)]
                                else:
                                    deltas = [TypeVarInt32.parse_stream(stream) for _ in range(n)]
                                data = con.Container(flags=bte, deltas=con.ListContainer(deltas[::2] if tah and tah.flags.TAENUM_64BIT else deltas))

                        else:   #struct & union
                            # list of type_t; [sdacl-typeattrs]; of size mcnt (technically union doesn't have sdacl but we should be good to call check anyways since sdacl bytes should be distinct from all valid base type_ts)

                            #align == -1 if use default align (processor specific; need additional logic in parsing.py (lumina-binja/lumina-ghidra))
                            align = -1 if (alpow:=(n & 0x7)) == 0 else (1 << (alpow - 1))
                            mcnt = n >> 3

                            tah = self.check_tah_or_sdacl(stream, typedef.basetype, path, True)   #sdacl

                            fields = []
                            for _ in range(mcnt):
                                tif = TypeInfo.parse_stream(stream)
                                sdacl = self.check_tah_or_sdacl(stream, tif, path, True)   #each field has its own sdacl-typeattr if it's structs, see comment above
                                fields.append(con.Container(type=tif, sdacl=sdacl))
                            data=con.Container(align=align, fields=con.ListContainer(fields))

            elif typedef.basetype == BaseTypes.BT_BITFIELD:
                val = TypeVarInt15.parse_stream(stream)
                data = con.Container(bitsize=val >> 1, unsigned=bool(val & 0b1)) #TODO abstract simple logic like these into construct?

            else: #should never happen once everything is implemented - unparsed data due to unknown type
                rest += con.GreedyBytes.parse_stream(stream)

        else: #all basic types may be followed by [tah-typeattrs]
            tah = self.check_tah_or_sdacl(stream, typedef.basetype, path)   

        return con.Container(typedef=typedef, typeattr=tah, data=data, unparsed=rest)

    def _build(self, obj, stream, context, path):
        return obj