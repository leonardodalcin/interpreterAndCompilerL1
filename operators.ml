#use "l1_def.ml"


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

