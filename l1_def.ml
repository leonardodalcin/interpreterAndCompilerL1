type variable = string

type operator = Sum | Diff | Mult | Div | Ge | Geq | Eq | Neq | Leq | Le

type tipo  = TyInt | TyBool | TyFn of tipo * tipo

type expr =
          | Num of int
          | Bool of bool
          | Bop of operator * expr * expr
          | If of expr * expr * expr
          | Var of variable
          | App of expr * expr
          | Lam of variable * tipo * expr
          | Let of variable * tipo * expr * expr
          | Lrec of variable * tipo * tipo * variable * tipo * expr * expr

type value = Vnum of int
           | Vbool of bool
           | Vclos of variable * expr * env
           | Vrclos of variable * variable * expr * env
and
     env = (variable * value) list


(* Except. *)


exception L1Exception of string;;


(* Value *)


let int_value value =
    match value with
    | Vnum(n) -> n
    | _ -> raise(L1Exception "Not a Vnum!");;


(* Env. *)


let extract_id t =
    match t with
    | (id, value) -> id;;


let extract_value t =
    match t with
    | (id, value) -> value;;


let rec update_env env id value =
    match env with
    | [] -> (id, value)::[]
    | hd :: tl ->
      let new_tl = update_env tl id value in
      if extract_id hd = id then new_tl else hd :: new_tl;;


let rec var_value env id =
    match env with
    | [] -> raise(L1Exception "Undeclared variable!")
    | hd :: tl ->
      if extract_id hd = id then extract_value hd else var_value tl id ;;


(* Print *)


let rec value_to_str value =
    match value with
        | Vbool(true) -> "TRUE"
        | Vbool(false) -> "FALSE"
        | Vnum(v) -> string_of_int v
        | Vrclos(f, x, expr, env) -> value_to_str (var_value env x)
        | _-> raise(L1Exception "value_to_str: Can't convert value to string!");;


let expr_to_str expr =
    match expr with
        | Bool(true) -> "TRUE"
        | Bool(false) -> "FALSE"
        | Num(v) -> string_of_int v
        | _-> raise(L1Exception "expr_to_str: Can't convert expression to string!");;


let print_value value =
    let val_str = value_to_str value in
    (Printf.printf "%s\n" val_str);;


let print_expr expr =
    let expr_str = expr_to_str expr in
    (Printf.printf "%s\n" expr_str);;
