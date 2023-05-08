namespace Fanna.Core.ByteCode

type ByteCode =
    | GetGlobal of (uint8 * uint8)
    | LoadConst of (uint8 * uint8)
    | Call of (uint8 * uint8)