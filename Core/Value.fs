namespace Fanna.Core.Value

type Value =
    /// Null value for Fanna
    | Nil

    /// The simplest string value. TODO: May be optimized later
    | String of string
    // | Function of (Exestate -> int32)

    override this.ToString() =
        match this with
        | Nil -> "nil"
        | String str -> $"""{str}"""
