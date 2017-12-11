#use "l1_def.ml"
#use "operators.ml"


let plus x1 x2 =
    match x1 with
            TmInteger(v1) ->
                (match x2 with
                        TmInteger(v2) -> TmInteger(v1 + v2)
                    |   _-> raise(Error "ERROR!"))
        |   _-> raise(Error "ERROR!");;


let minus x1 x2 =
    match x1 with
            TmInteger(v1) ->
                (match x2 with
                        TmInteger(v2) -> TmInteger(v1 - v2)
                    |   _-> raise(Error "ERROR!"))
        |   _-> raise(Error "ERROR!");;


let mult x1 x2 =
    match x1 with
            TmInteger(v1) ->
                (match x2 with
                        TmInteger(v2) -> TmInteger(v1 * v2)
                    |   _-> raise(Error "ERROR!"))
        |   _-> raise(Error "ERROR!");;


let div x1 x2 =
    match x1 with
            TmInteger(v1) ->
                (match x2 with
                        TmInteger(v2) -> TmInteger(v1 / v2)
                    |   _-> raise(Error "ERROR!"))
        |   _-> raise(Error "ERROR!");;


let lesser x1 x2 =
    match x1 with
            TmInteger(v1) ->
                (match x2 with
                        TmInteger(v2) -> (if v1 < v2 then TmTrue else TmFalse)
                    |   _-> raise(Error "ERROR!"))
        |   _-> raise(Error "ERROR!");;


let lesser_equal x1 x2 =
    match x1 with
            TmInteger(v1) ->
                (match x2 with
                        TmInteger(v2) -> (if v1 <= v2 then TmTrue else TmFalse)
                    |   _-> raise(Error "ERROR!"))
        |   _-> raise(Error "ERROR!");;


let equal x1 x2 =
    match x1 with
            TmInteger(v1) ->
                (match x2 with
                        TmInteger(v2) -> (if v1 == v2 then TmTrue else TmFalse)
                    |   _-> raise(Error "ERROR!"))
        |   _-> raise(Error "ERROR!");;


let not_equal x1 x2 =
    match x1 with
            TmInteger(v1) ->
                (match x2 with
                        TmInteger(v2) -> (if v1 != v2 then TmTrue else TmFalse)
                    |   _-> raise(Error "ERROR!"))
        |   _-> raise(Error "ERROR!");;


let greater x1 x2 =
    match x1 with
            TmInteger(v1) ->
                (match x2 with
                        TmInteger(v2) -> (if v1 > v2 then TmTrue else TmFalse)
                    |   _-> raise(Error "ERROR!"))
        |   _-> raise(Error "ERROR!");;


let greater_equal x1 x2 =
    match x1 with
            TmInteger(v1) ->
                (match x2 with
                        TmInteger(v2) -> (if v1 >= v2 then TmTrue else TmFalse)
                    |   _-> raise(Error "ERROR!"))
        |   _-> raise(Error "ERROR!");;


let rec big_step t =
    match t with
            TmInteger(v) -> TmInteger(v)
        |   TmTrue -> TmTrue
        |   TmFalse -> TmFalse
        |   TmIf(t1 ,t2 ,t3) ->
                let t1' = big_step t1 in
                let t2' = big_step t2 in
                let t3' = big_step t3 in
                    (match t1' with
                            TmTrue -> t2'
                        |   TmFalse -> t3'
                        |   _-> raise(Error "TmIf: Invalid t1'!"))
        |   TmPlus(t1, t2) ->
                let t1' = big_step t1 in
                let t2' = big_step t2 in
                plus t1' t2'
        |   TmMinus(t1, t2) ->
                let t1' = big_step t1 in
                let t2' = big_step t2 in
                minus t1' t2'
        |   TmMult(t1, t2) ->
                let t1' = big_step t1 in
                let t2' = big_step t2 in
                mult t1' t2'
        |   TmDiv(t1, t2) ->
                let t1' = big_step t1 in
                let t2' = big_step t2 in
                div t1' t2'
        |   TmLesser(t1, t2) ->
                let t1' = big_step t1 in
                let t2' = big_step t2 in
                lesser t1' t2'
        |   TmLesserOrEqual(t1, t2) ->
                let t1' = big_step t1 in
                let t2' = big_step t2 in
                lesser_equal t1' t2'
        |   TmGreater(t1, t2) ->
                let t1' = big_step t1 in
                let t2' = big_step t2 in
                greater t1' t2'
        |   TmGreaterOrEqual(t1, t2) ->
                let t1' = big_step t1 in
                let t2' = big_step t2 in
                greater_equal t1' t2'
        |   TmEqual(t1, t2) ->
                let t1' = big_step t1 in
                let t2' = big_step t2 in
                equal t1' t2'
        |   TmNotEqual(t1, t2) ->
                let t1' = big_step t1 in
                let t2' = big_step t2 in
                not_equal t1' t2'
        |   _-> raise (Error "ERROR!");;
