import socket
import construct as con
from construct import (
    Byte, Bytes, Int8ub, Int16ub, Int16ul, Int16sb, Int32ub, Int32ul, Int64ub,
    CString, Hex,
    Struct, Array, Const, Rebuild, len_, this, FormatField,
    Container,
    )

from .basetypes import *
from .metadata import MetadataPayload, MetadataType

IDA_PROTOCOL_VERSION = 2


#######################################
#
# Lumina types
#
#######################################

# function signature
func_sig_t = con.Struct(
    "version" / Const(1, IdaVarInt32),  # protocol version (con.Default: 1)
    "signature" / VarBuff               # signature buffer
    )

# a.k.a func_info_t
func_metadata = con.Struct(
    "func_name" / CString("utf8"),      # function name
    "func_size" / IdaVarInt32,          # function size in bytes
    "serialized_data" / MetadataPayload # metadata
    )

# extended func_metadata
func_info_t = con.Struct(
    "metadata" / func_metadata,                   #
    "popularity" / con.Default(IdaVarInt32, 0),   # unknown
    )

func_md_t = con.Struct(
    "metadata" / func_metadata,
    "signature" / func_sig_t
    )

# same as func_md_t with extra (unknown) field
func_md2_t = con.Struct(
    "metadata" / func_metadata,
    "signature" / func_sig_t,
    "field_0x58" / Hex(Const(0, IdaVarInt32)),
    )

#######################################
#
# Lumina message types
#
#######################################

RPC_TYPE = con.Enum(Byte,
    RPC_OK = 0xa,
    RPC_FAIL = 0xb,
    RPC_NOTIFY = 0xc,
    RPC_HELO = 0xd,
    PULL_MD = 0xe,
    PULL_MD_RESULT = 0xf,
    PUSH_MD = 0x10,
    PUSH_MD_RESULT = 0x11,
    # below messages are not implemented or not used by Lumina. Enjoy yourselves ;)
    GET_POP = 0x12,
    GET_POP_RESULT = 0x13,
    LIST_PEERS = 0x14,
    LIST_PEERS_RESULT = 0x15,
    KILL_SESSIONS = 0x16,
    KILL_SESSIONS_RESULT = 0x17,
    DEL_ENTRIES = 0x18,
    DEL_ENTRIES_RESULT = 0x19,
    SHOW_ENTRIES = 0x1a,
    SHOW_ENTRIES_RESULT = 0x1b,
    DUMP_MD = 0x1c,
    DUMP_MD_RESULT = 0x1d,
    CLEAN_DB = 0x1e,
    DEBUGCTL = 0x1f
)

ResultType = con.Enum(IdaVarInt32,                 # basically only NOT_FOUND, OK and ADDED are used
	RES_BADPTN = 0xFFFFFFFD,
	RES_NOT_FOUND = 0xFFFFFFFE,
	RES_ERROR = 0xFFFFFFFF,
	RES_OK = 0x0,
	RES_ADDED = 0x1,
)

RpcMessage_FAIL = con.Struct(
    "status" / IdaVarInt32,
    "message" / CString("utf-8"),                   # null terminated string
)

RpcMessage_HELO = con.Struct(
    "protocol" / con.Default(IdaVarInt32, IDA_PROTOCOL_VERSION),
    "hexrays_license" / VarBuff,                    # ida.key file content
    "hexrays_id" / Hex(Bytes(6)),                   # 6 byte array representing IDAPRO*W LICENSE_ID
    "field_0x36" / IdaVarInt32,                     # always zero ?
)

RpcMessage_NOTIFY = con.Struct(
    "protocol" / con.Default(IdaVarInt32, IDA_PROTOCOL_VERSION),
    "message" / CString("utf-8"),                   # null terminated string
)

RpcMessage_PULL_MD  = con.Struct(
    "flags" / IdaVarInt32,                          # on IDA 7.5 / protocol 2, this is hardcoded to be 1 regardless of arch (the lumina pull_md routine in ida.dll expects lowermost bit to be 1 or else it will not apply fetched metadata, so it's certainly not type IDs either)
    "types" / ObjectList(MetadataType),             # list of MetadataTypes; seems to always be empty normally? might be used for retrieving only specific metadata
    "funcInfos" / ObjectList(func_sig_t)            # list of func_sig_t
)

RpcMessage_PULL_MD_RESULT = con.Struct(
    "found" / ObjectList(ResultType),               # list of ResultTypes for each request in PULL_MD
    "results" / ObjectList(func_info_t)             # list of func_info_t for each matching result
)


PushMdOpt = con.Enum(IdaVarInt32,
	PUSH_OVERRIDE_IF_BETTER = 0x0,
	PUSH_OVERRIDE = 0x1,
	PUSH_DO_NOT_OVERRIDE = 0x2,
	PUSH_MERGE = 0x3,
)

RpcMessage_PUSH_MD = con.Struct(
    "type" / PushMdOpt,                             # for IDA 7.5 / protocol 2, this is hardcoded to be 0
    "idb_filepath" / CString("utf-8"),              # absolute file path of current idb
    "input_filepath" / CString("utf-8"),            # absolute file path of input file
    "input_md5" / Bytes(16),                        # input file md5
    "hostname" / CString("utf-8"),                  # machine name
    "funcInfos" / ObjectList(func_md_t),            # list of func_md_t to push
    "funcEas" / ObjectList(IdaVarInt64),            # absolute (!?) address of each pushed function
)


RpcMessage_PUSH_MD_RESULT = con.Struct(
    "resultsFlags" / ObjectList(ResultType),       # status for each function pushed
)



# Generic RPC message 'union'
RpcMessage = con.Switch(this.code,
        {
            RPC_TYPE.RPC_OK : con.Pass,
            RPC_TYPE.RPC_FAIL : RpcMessage_FAIL,
            RPC_TYPE.RPC_NOTIFY : RpcMessage_NOTIFY,
            RPC_TYPE.RPC_HELO : RpcMessage_HELO,
            RPC_TYPE.PULL_MD : RpcMessage_PULL_MD,
            RPC_TYPE.PULL_MD_RESULT : RpcMessage_PULL_MD_RESULT,
            RPC_TYPE.PUSH_MD : RpcMessage_PUSH_MD,
            RPC_TYPE.PUSH_MD_RESULT : RpcMessage_PUSH_MD_RESULT,
            #RPC_TYPE.GET_POP : RpcMessage_GET_POP,
            #RPC_TYPE.GET_POP_RESULT : RpcMessage_GET_POP_RESULT,
            #RPC_TYPE.LIST_PEERS : RpcMessage_LIST_PEERS,
            #RPC_TYPE.LIST_PEERS_RESULT : RpcMessage_LIST_PEERS_RESULT,
            #RPC_TYPE.KILL_SESSIONS : RpcMessage_KILL_SESSIONS,
            #RPC_TYPE.KILL_SESSIONS_RESULT : RpcMessage_KILL_SESSIONS_RESULT,
            #RPC_TYPE.DEL_ENTRIES : RpcMessage_DEL_ENTRIES,
            #RPC_TYPE.DEL_ENTRIES_RESULT : RpcMessage_DEL_ENTRIES_RESULT,
            #RPC_TYPE.SHOW_ENTRIES : RpcMessage_SHOW_ENTRIES,
            #RPC_TYPE.SHOW_ENTRIES_RESULT : RpcMessage_SHOW_ENTRIES_RESULT,
            #RPC_TYPE.DUMP_MD : RpcMessage_DUMP_MD,
            #RPC_TYPE.DUMP_MD_RESULT : RpcMessage_DUMP_MD_RESULT,
            #RPC_TYPE.CLEAN_DB : RpcMessage_CLEAN_DB,
            #RPC_TYPE.DEBUGCTL : RpcMessage_DEBUGCTL,
        },
        default = None
    )

# RPC packet common header
rpc_packet_t = con.Struct(
    "length" / Rebuild(Hex(Int32ub), len_(this.data)),
    "code" / RPC_TYPE,
    "data" / con.HexDump(con.Bytes(this.length))
    )

def rpc_message_build(code, **kwargs):
    """
    Build and serialize an RPC packet
    """
    data = RpcMessage.build(kwargs, code = code)

    return rpc_packet_t.build(Container(code = code,
        data = data)
    )

def rpc_message_parse(source):
    """
    Read and deserialize RPC message from a file-like object or socket)
    """
    if isinstance(source, str):
        # parse source as filename
        packet = rpc_packet_t.parse_stream(source)
    elif isinstance(source, bytes):
        # parse source as bytes
        packet = rpc_packet_t.parse(source)
    else:
        # parse source as file-like object
        if isinstance(source, socket.socket):
            # construct requires a file-like object with read/write methods:
            source = source.makefile(mode='rb')

        packet = rpc_packet_t.parse_stream(source)

    message = RpcMessage.parse(packet.data , code = packet.code)
    # Warning: parsing return a Container object which hold a io.BytesIO to the socket
    # see https://github.com/construct/construct/issues/852
    return packet, message
