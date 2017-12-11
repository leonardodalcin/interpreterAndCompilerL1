exception Error of string;;

type term =
    |   TmInteger of int
    |   TmBool
    |   TmTrue
    |   TmFalse
    |   TmPlus of term * term
    |   TmMinus of term * term
    |   TmMult of term * term
    |   TmDiv of term * term
    |   TmLesser of term * term
    |   TmLesserOrEqual of term * term
    |   TmEqual of term * term
    |   TmNotEqual of term * term
    |   TmGreater of term * term
    |   TmGreaterOrEqual of term * term
    |   TmIf of term * term * term
    |   TmX of term
    |   TmApp of term * term
    |   TmFn of term
    |   TmLetX of term
    |   TmLetRec of term
    |   TmIdent of string


let to_str t =
    match t with
            TmTrue -> "TmTrue"
        |   TmFalse -> "TmFalse"
        |   TmInteger(v) -> string_of_int v
        |   _-> raise(Error "ERROR!");;


let print_tm t =
    let t_str = to_str t in
    (Printf.printf "%s\n" t_str)

