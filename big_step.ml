#use "l1_def.ml"

let rec big_step t = match t with

        TmTrue -> true
    |   TmFalse -> false
    |   _ -> false;;