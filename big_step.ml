#use "l1_def.ml"


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


(* Big step *)


let rec big_step exp env =
    match exp with
    | Num(int_value) -> Vnum(int_value)
    | Bool(bool_value) -> Vbool(bool_value)
    | Bop(operator, exp1, exp2) ->
        let v1 = big_step exp1 env in
        let v2 = big_step exp2 env in
        (match operator with
        | Sum -> Vnum((int_value v1) + (int_value v2))
        | Diff -> Vnum((int_value v1) - (int_value v2))
        | Mult -> Vnum((int_value v1) * (int_value v2))
        | Div -> Vnum((int_value v1) / (int_value v2))
        | Ge -> Vbool((int_value v1) > (int_value v2))
        | Geq -> Vbool((int_value v1) >= (int_value v2))
        | Eq -> Vbool((int_value v1) == (int_value v2))
        | Neq -> Vbool((int_value v1) != (int_value v2))
        | Leq -> Vbool((int_value v1) <= (int_value v2))
        | Le -> Vbool((int_value v1) < (int_value v2)))
    | If(cond, exp1, exp2) ->
        let cond' = big_step cond env in
        (match cond' with
        | Vbool(true) -> big_step exp1 env
        | Vbool(false) -> big_step exp2 env
        | _-> raise(L1Exception "If: invalid t1 type."))
    | Var(var) -> var_value env var
    | App(exp1, exp2) ->
        let clos = big_step exp1 env in
        let v' = big_step exp2 env in
        (match clos with
        | Vclos(x, e, env') ->
            let env' = update_env env' x v' in
            big_step e env'
        | Vrclos(f, x, e, env') ->
            let env' = update_env env' x v' in
            let env' = update_env env' f clos in
            big_step e env'
        | _-> raise(L1Exception "Not a closure!"))
    | Lam(var, var_type, exp) -> Vclos(var, exp, env)
    | Let(var, var_type, exp1, exp2) ->
        let var_val = big_step exp1 env in
        let env' = update_env env var var_val in
        big_step exp2 env'
    | Lrec(f_name, in_type, out_type, arg, arg_type, exp1, exp2) ->
        let f_clos = Vrclos(f_name, arg, exp1, env) in
        let env' = update_env env f_name f_clos in
        big_step exp2 env'


(* Eval. *)


let rec eval exp =
    let env = [] in
    big_step exp env
