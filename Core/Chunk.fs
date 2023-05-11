module Fanna.Core.Chunk

module Const =
    let SIGNATURE = uint32 (0x1b4c7561)
    let VERSION = 0x53uy
    let LUAC_FORMAT = 0uy
    let LUAC_DATE = [| 0x19uy; 0x93uy; 0x0duy; 0x0auy; 0x1auy; 0x0auy |]
    let CINT_SIZE = 4uy
    let CSIZET_SIZE = 8uy
    let INSTRUCTION_SIZE = 4uy
    let LUA_INTEGER_SIZE = 8uy
    let LUA_NUMBER_SIZE = 8uy
    let LUAC_INT = 0x5678L
    let LUAC_NUM = 370.5


type Header
    (
        signature,
        version,
        format,
        luacData,
        cintSize,
        sizetSize,
        instructionSize,
        luaIntegerSize,
        luaNumberSize,
        luacInt,
        luacNum
    ) =

    /// Signature is used to quickly identify the file format.
    /// Lua's binary chunk signature is 4 bytes, expressed in hexadecimal is Ox1B4C7561
    /// If the Lua virtual machine tries to load a binary chunk file and finds that it does not start with OxlB4C7561,
    /// it will refuse to load the file
    member public _.Signature: array<byte> = signature

    /// A byte after the signature records the Lua version number corresponding to the binary chunk file.
    /// The version number of Lua language consists of three parts: Major Version, Minor Version, Release Version.
    /// The version number stored in the binary chunk is calculated based on the Lua major and minor version number,
    /// and its value is equal to (major version * 16 + minor version).
    /// The reason why the release number is not considered is because the increase in the release number only means bug fixes
    /// and does not No adjustments are made to the binary chunk format.
    /// When the Lua virtual machine loads the binary chunk, it will check its version number,
    /// and if it does not match the version number of the virtual machine itself, it will refuse to load the file.
    member public _.Version: byte = version

    /// A byte after the version number records the binary chunk format number.
    /// When the Lua virtual machine loads a binary chunk, it will check its format number,
    /// and if it does not match the format number of the virtual machine itself, it will refuse to load the file.
    /// The format number used by the official Lua implementation is 0
    member public _.Format: byte = format

    /// The 6 bytes after the format number are called LUAC DATA in the official Lua implementation.
    /// Among them, the first two bytes are Ox1993, which is the year when Lua 1.0 was released.
    /// The last 4 bytes are carriage return ( OxOD ), line feed ( OxOA ), replacement ( OxlA ) and another line feed.
    /// These 6 bytes are mainly used for further verification.
    /// If the Lua virtual machine finds that the 6 bytes are different from the expected one when loading the binary chunk,
    /// it will consider the file damaged and refuse to load it.
    member public _.LuacData: array<byte> = luacData

    /// The next 5 bytes respectively record the number of bytes occupied by the five data types of
    /// cint, size_t, Lua virtual machine instruction, Lua integer and Lua floating point number in the binary chunk.
    /// When the Lua virtual machine loads a binary chunk, it will check the number of bytes occupied by the above five data types,
    /// and refuse to load if it does not match the expected value.
    member public _.CintSize: byte = cintSize
    member public _.SizetSize: byte = sizetSize
    member public _.InstructionSize: byte = instructionSize
    member public _.LuaIntegerSize: byte = luaIntegerSize
    member public _.LuaNumberSize: byte = luaNumberSize

    /// The next LuaIntegerSize bytes hold the Lua integer value Ox5678.
    /// The purpose of storing this Lua integer is to detect the endianness of the binary chunk.
    /// When the Lua virtual machine loads a binary chunk,
    /// it will use this data to check whether its big and small endian mode matches the local machine,
    /// and if it does not match, it will refuse to load
    member public _.LuacInt: int64 = luacInt

    /// The last LuaNumberSize bytes store the Lua floating point number 370.5.
    /// The purpose of storing this Lua floating-point number is to detect the floating-point number format used by the binary chunk.
    /// When the Lua virtual machine loads a binary chunk,
    /// it will use this data to check whether its floating-point number format matches the local machine,
    /// and if it does not match, it will refuse to load.
    /// Currently mainstream platforms and languages ​​generally adopt the IEEE 754 floating-point format.
    member public _.LuacNum: float = luacNum


/// The function prototype mainly includes function basic information, instruction table, constant table, upvalue table, sub-function prototype table and debugging information
/// Basic information includes source file name, start and end line numbers, number of fixed parameters, whether it is a vararg function, and the number of registers necessary to run the function
/// Debug information also includes a line number table, a local variable table, and a list of upvalue names
type ProtoType
    (
        source,
        lineDefined,
        lastLineDefined,
        numParams,
        isVararg,
        maxStackSize,
        code,
        constants,
        upvalues,
        protos,
        lineInfo,
        locaVars,
        UpvalueNames
    ) =
    /// The first field of the function prototype stores the source file name,
    /// and records which source file the binary chunk is compiled from.
    /// In order to avoid repetition, only in the prototype of the main function,
    /// this field has a real value, and in other nested function prototypes, this field stores an empty string.
    /// The source filename is not information required to execute the function.
    /// If compiled with the "-s" option, the source file name will be stripped from the binary chunk by the Lua compiler along with other debugging information.
    member public _.Source: string = source

    /// Following the source file name are two integers,
    /// which are used to record the start and end line numbers of the function corresponding to the prototype in the source file.
    /// If it is an ordinary function, the start and end line numbers should be greater than O; if it is a main function, the start and end line numbers are both 0
    member public _.LineDefined: uint32 = lineDefined
    member public _.LastLineDefined: uint32 = lastLineDefined

    /// A byte after the start and end line numbers records the number of fixed parameters of the function.
    /// The Lua compiler has no fixed parameters for the generated main function, so for main, this value is 0
    member public _.NumParams: byte = numParams

    /// The next byte is used to record whether the function is a Vararg function,
    /// the main function is a Vararg function, and has variable length parameters, so the value for main is 1
    member public _.IsVararg: byte = isVararg

    /// The next byte records the number of registers.
    /// The Lua compiler generates bytecode for each Lua function.
    /// Since the Lua virtual machine is a register-based virtual machine,
    /// the Lua compiler will calculate how many registers the function needs to use when compiling the function, and save it in byte type.
    member public _.MaxStackSize: byte = maxStackSize

    /// After that is the instruction table, each instruction occupies 4 bytes
    member public _.Code: array<uint32> = code

    /// Following the instruction list is the constant list.
    /// The constant table is used to store the literals appearing in Lua code, including nil, Boolean value, integer, floating point number and string.
    /// Each constant starts with a 1-byte tag, which is used to identify which type of constant value is stored subsequently.
    member public _.Constants: unit = constants

    /// Constant table followed by Upvalue table
    /// Each element occupies 2 bytes
    member public _.Upvalues: array<Upvalue> = upvalues

    /// After the Upvalue table is the subfunction prototype table
    member public _.Protos: array<ProtoType> = protos

    /// After the subfunction prototype table is the line number table, where the line number is stored in cint type.
    /// The line numbers in the line number table are in one-to-one correspondence with the instructions in the instruction table,
    /// and the line numbers corresponding to each instruction in the source code are recorded respectively.
    member public _.LineInfo: array<uint32> = lineInfo

    /// After the line number table is the local variable table,
    /// which is used to record the local variable name.
    /// Each element in the table contains the variable name (stored by string type) and start and stop instruction index (stored by cint type).
    member public _.LocVars: array<LocVar> = locaVars

    /// The last part of the function prototype is the list of Upvalue names.
    /// The elements in this list (stored in string type) correspond to the elements in the previous Upvalue table one by one,
    /// and record the name of each Upvalue in the source code respectively.
    member public _.UpvalueNames: array<string> = UpvalueNames

and Upvalue(inStack, idx) =
    member public _.Instack: byte = inStack
    member public _.Idx: byte = idx

and LocVar(varName, startPC, endPC) =
    member public _.VarName: string = varName
    member public _.StartPC: uint32 = startPC
    member public _.EndPC: uint32 = endPC

/// Lua's binary chunk is essentially a byte stream.
/// The binary chunk format (including Lua virtual machine instructions) is an internal implementation detail of the Lua virtual machine.
/// It is not standardized, and there is no official document explaining it.
/// Everything is subject to the source code of the official Lua implementation.
/// The design of the binary chunk format does not consider cross-platform requirements.
/// For data that needs to be represented by more than one byte, the issue of endianness must be considered.
/// The design of the binary chunk format does not consider compatibility issues between different Lua versions.
/// The binary chunk format is not intentionally compact.
type BinaryChunk(header, sizeUpvalues, mainFunc) =
    member public _.Header: Header = header
    member public _.SizeUpvalues: byte = sizeUpvalues
    member public _.MainFunc: ProtoType = mainFunc

    static member private CheckHeader(data: Utils.ByteStreamReader) =
        if data.ReadUint32() <> Const.SIGNATURE then
            failwith ("not a lua precompiled chunk!")
        else if data.ReadByte() <> Const.VERSION then
            failwith ("version mismatch!")
        else if data.ReadByte() <> Const.LUAC_FORMAT then
            failwith ("format mismatch!")
        else if data.ReadBytes(6) <> Const.LUAC_DATE then
            failwith ("corrupted!")
        else if data.ReadByte() <> Const.CINT_SIZE then
            failwith ("int size mismatch!")
        else if data.ReadByte() <> Const.CSIZET_SIZE then
            failwith ("size_t size mismatch!")
        else if data.ReadByte() <> Const.INSTRUCTION_SIZE then
            failwith ("instruction size mismatch!")
        else if data.ReadByte() <> Const.LUA_INTEGER_SIZE then
            failwith ("lua integer size mismatch!")
        else if data.ReadByte() <> Const.LUA_NUMBER_SIZE then
            failwith ("lua number size mismatch!")
        else if data.ReadLuaInteger() <> Const.LUAC_INT then
            failwith ("endianness mismatch!")
        else if data.ReadLuaNumber() <> Const.LUAC_NUM then
            failwith ("float format mismatch!")
        else
            data

    /// The number of Upvalue of the main function can also be obtained from the prototype of the main function, so skip this field for now
    static member private SkipUpvalueNum(data: Utils.ByteStreamReader) = data

    static member private ReadProto(data: Utils.ByteStreamReader) = data

    static member Undump(data: array<byte>) =
        Utils.ByteStreamReader(data)
        |> BinaryChunk.CheckHeader
        |> BinaryChunk.SkipUpvalueNum
        |> BinaryChunk.ReadProto