/// Each Lua virtual machine instruction occupies 4 bytes, a total of 32 bits.
/// The lower 6 bits are used for the opcode and the upper 26 bits are used for the operand.
/// According to the allocation method of the upper 26 bits, Lua virtual machine instructions can be divided into four categories,
/// corresponding to four encoding modes: iABC , iABx , iAsBx , iAx.
module Fanna.Core.Instruction

module Const =
    let MAXARG_Bx = uint32 (1 <<< 18 - 1)
    let MAXARG_sBx = uint32 (MAXARG_Bx >>> 1)

/// iABC: carry three operands, A, B, and C, occupying 8, 9, and 9 bits respectively.
/// iABx: carry two operands, A and Bx, occupying 8 and 18 bits respectively.
/// iAsBx: carry two operands, A and sBx, occupying 8 and 18 bits respectively.
/// iAx: carries one operand, occupying all 26 bits.
/// Among the 4 modes, only the sBx operand in iAsBx mode will be interpreted as a signed integer,
/// and the operand in other cases will be interpreted as an unsigned integer.
type InstructionMode =
    | IABC
    | IABx
    | IAsBx
    | IAx

/// Opcodes are used to identify instructions.
/// Since the Lua virtual machine instructions use 6 bits to represent the opcode, there can only be a maximum of 64 instructions.
/// Lua 5.3 defines a total of 47 instructions, with opcodes starting from 0 and ending at 46.
type OpCode =
    | OP_MOVE
    | OP_LOADK
    | OP_LOADNIL
    | OP_GETUPVAL
    | OP_LOADKX
    | OP_GETTABUP
    | OP_LOADBOOL
    | OP_GETTABLE
    | OP_SETTABUP
    | OP_SETTABLE
    | OP_NEWTABLE
    | OP_SETUPVAL
    | OP_SELF
    | OP_ADD
    | OP_SUB
    | OP_MUL
    | OP_MOD
    | OP_POW
    | OP_BAND
    | OP_BOR
    | OP_BXOR
    | OP_SHL
    | OP_SHR
    | OP_UNM
    | OP_BNOT
    | OP_NOT
    | OP_LEN
    | OP_CONCAT
    | OP_JMP
    | OP_EQ
    | OP_LT
    | OP_LE
    | OP_TEST
    | OP_TESTSET
    | OP_CALL
    | OP_TAILCALL
    | OP_RETURN
    | OP_FORLOOP
    | OP_FORPREP
    | OP_TFORCALL
    | OP_TFORLOOP
    | OP_SETLIST
    | OP_CLOSURE
    | OP_VARARG
    | OP_EXTRAARG


/// Operands are arguments to instructions, and each instruction (varies by encoding mode) can carry from l to 3 operands.
/// Among them, the operand a is mainly used to indicate the index of the target register, and the other operands follow the information it represents
type OpArgMode =
    /// Does not indicate any information
    | Arg_N

    /// Represents boolean values, integer values, upvalue indices, subfunction indices, etc.
    | Arg_U

    /// In iABC mode, it represents the register index, and in iAsBx mode, it represents the jump offset.
    | Arg_R

    /// Indicates constant table index or register index, which can be divided into two cases:
    /// The first case is the LOADK instruction (iABx mode, used to load constants from the constant table into registers), whose Bx operand represents the constant table index.
    /// The second case is some iABC mode instructions whose B or C operand can represent either a constant table index or a register index.
    /// In iABC mode, operands B and C each occupy 9 bits.
    /// If operand B or C belongs to the type of OpArgK, then only the lower 8 bits of the 9 bits can be used.
    /// If the highest bit is 1, then The operand represents a constant table index, otherwise a register index.
    | Arg_K


/// The official implementation of Lua encodes the basic information of each instruction into 1 byte.
/// And record the name of the opcode.
type Instruction(testFlag, setAFlag, argBMode, argCMode, instructionMode, name) =

    /// Operator is a test (next instruction must be a jump)
    member public _.TestFlag: byte = testFlag

    /// Instruction set register A
    member public _.SetAFlag: byte = setAFlag

    /// B arg mode
    member public _.ArgBMode: OpArgMode = argBMode

    /// C arg mode
    member public _.ArgCMode: OpArgMode = argCMode

    /// Instruction Mode
    member public _.InstructionMode: InstructionMode = instructionMode

    /// The name of the opcode
    member public _.Name: string = name

    /// Complete instruction list
    static member public INSTRUCTIONS =
        [| Instruction(0uy, 1uy, Arg_R, Arg_N, IABC, "MOVE     ")
           Instruction(0uy, 1uy, Arg_K, Arg_N, IABx, "LOADK    ")
           Instruction(0uy, 1uy, Arg_N, Arg_N, IABx, "LOADKX  ")
           Instruction(0uy, 1uy, Arg_U, Arg_U, IABC, "LOADBOOL")
           Instruction(0uy, 1uy, Arg_U, Arg_N, IABC, "LOADNIL ")
           Instruction(0uy, 1uy, Arg_U, Arg_N, IABC, "GETUPVAL")
           Instruction(0uy, 1uy, Arg_U, Arg_K, IABC, "GETTABUP")
           Instruction(0uy, 1uy, Arg_R, Arg_K, IABC, "GETTABLE")
           Instruction(0uy, 0uy, Arg_K, Arg_K, IABC, "SETTABUP")
           Instruction(0uy, 0uy, Arg_U, Arg_N, IABC, "SETUPVAL")
           Instruction(0uy, 0uy, Arg_K, Arg_K, IABC, "SETTABLE")
           Instruction(0uy, 1uy, Arg_U, Arg_U, IABC, "NEWTABLE")
           Instruction(0uy, 1uy, Arg_R, Arg_K, IABC, "SELF     ")
           Instruction(0uy, 1uy, Arg_K, Arg_K, IABC, "ADD      ")
           Instruction(0uy, 1uy, Arg_K, Arg_K, IABC, "SUB      ")
           Instruction(0uy, 1uy, Arg_K, Arg_K, IABC, "MUL      ")
           Instruction(0uy, 1uy, Arg_K, Arg_K, IABC, "MOD      ")
           Instruction(0uy, 1uy, Arg_K, Arg_K, IABC, "POW      ")
           Instruction(0uy, 1uy, Arg_K, Arg_K, IABC, "DIV      ")
           Instruction(0uy, 1uy, Arg_K, Arg_K, IABC, "IDIV     ")
           Instruction(0uy, 1uy, Arg_K, Arg_K, IABC, "BAND     ")
           Instruction(0uy, 1uy, Arg_K, Arg_K, IABC, "BOR      ")
           Instruction(0uy, 1uy, Arg_K, Arg_K, IABC, "BXOR     ")
           Instruction(0uy, 1uy, Arg_K, Arg_K, IABC, "SHL      ")
           Instruction(0uy, 1uy, Arg_K, Arg_K, IABC, "SHR      ")
           Instruction(0uy, 1uy, Arg_R, Arg_N, IABC, "UNM      ")
           Instruction(0uy, 1uy, Arg_R, Arg_N, IABC, "BNOT     ")
           Instruction(0uy, 1uy, Arg_R, Arg_N, IABC, "NOT      ")
           Instruction(0uy, 1uy, Arg_R, Arg_N, IABC, "LEN      ")
           Instruction(0uy, 1uy, Arg_R, Arg_R, IABC, "CONCAT  ")
           Instruction(0uy, 0uy, Arg_R, Arg_N, IAsBx, "JMP      ")
           Instruction(1uy, 0uy, Arg_K, Arg_K, IABC, "EQ       ")
           Instruction(1uy, 0uy, Arg_K, Arg_K, IABC, "LT       ")
           Instruction(1uy, 0uy, Arg_K, Arg_K, IABC, "LE       ")
           Instruction(1uy, 0uy, Arg_N, Arg_U, IABC, "TEST     ")
           Instruction(1uy, 1uy, Arg_R, Arg_U, IABC, "TESTSET ")
           Instruction(0uy, 1uy, Arg_U, Arg_U, IABC, "CALL     ")
           Instruction(0uy, 1uy, Arg_U, Arg_U, IABC, "TAILCALL")
           Instruction(0uy, 0uy, Arg_U, Arg_N, IABC, "RETURN  ")
           Instruction(0uy, 1uy, Arg_R, Arg_N, IAsBx, "FORLOOP ")
           Instruction(0uy, 1uy, Arg_R, Arg_N, IAsBx, "FORPREP ")
           Instruction(0uy, 0uy, Arg_N, Arg_U, IABC, "TFORCALL")
           Instruction(0uy, 1uy, Arg_R, Arg_N, IAsBx, "TFORLOOP")
           Instruction(0uy, 0uy, Arg_U, Arg_U, IABC, "SETLIST ")
           Instruction(0uy, 1uy, Arg_U, Arg_N, IABx, "CLOSURE ")
           Instruction(0uy, 1uy, Arg_U, Arg_N, IABC, "VARARG  ")
           Instruction(0uy, 0uy, Arg_U, Arg_U, IAx, "EXTRAARG") |]

/// Instructions stored in BinaryChunk
type ChunkInstruction(instruction: uint32) =
    member public _.OpCode = instruction &&& 0x3Fu
    member public this.Name = Instruction.INSTRUCTIONS[int (this.OpCode)].Name
    member public this.Mode = Instruction.INSTRUCTIONS[int (this.OpCode)].InstructionMode
    member public this.BMode = Instruction.INSTRUCTIONS[int (this.OpCode)].ArgBMode
    member public this.CMode = Instruction.INSTRUCTIONS[int (this.OpCode)].ArgCMode

    member public _.ABC() =
        (instruction >>> 6 &&& 0xFFu, instruction >>> 14 &&& 0x1FFu, instruction >>> 23 &&& 0x1FFu)

    member public _.ABx() =
        (instruction >>> 6 &&& 0xFFu, instruction >>> 14)

    /// The sBx operand (18 bits in total) represents a signed integer.
    /// The Lua virtual machine adopts the Ecess-K encoding mode,
    /// and K takes half of the largest unsigned integer value that sBx can represent, that is, MAXARG_sBx
    member public this.AsBx() =
        let (a, bx) = this.ABx()
        (a, bx - Const.MAXARG_sBx)

    member public _.Ax() = instruction >>> 6