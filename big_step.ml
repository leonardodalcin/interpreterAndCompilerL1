#use "l1_def.ml"



exception L1Exception of string;;


(* Values *)


let int_value exp =
    match exp with
    | Vnum(v) -> v
    | _-> raise(L1Exception "Not an int value.");;


(* Print *)


let value_to_str value =
    match value with
        | Vbool(true) -> "TRUE"
        | Vbool(false) -> "FALSE"
        | Vnum(v) -> string_of_int v
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


(* Clos. *)


let rec extract_clos_env clos =
    match clos with
    | Vrclos(f, arg, expr, env) -> env
    | _-> raise(L1Exception "Not a closure!");;


let rec extract_clos_arg clos =
    match clos with
    | Vrclos(f, arg, expr, env) -> arg
    | _-> raise(L1Exception "Not a closure!");;


let rec extract_clos_f clos =
    match clos with
    | Vrclos(f, arg, expr, env) -> f
    | _-> raise(L1Exception "Not a closure!");;


let rec extract_clos_expr clos =
    match clos with
    | Vrclos(f, arg, expr, env) -> expr
    | _-> raise(L1Exception "Not a closure!");;


let rec change_clos_env clos new_env =
    match clos with
    | Vrclos(f, arg, expr, env) -> Vrclos(f, arg, expr, new_env)
    | _-> raise(L1Exception "Not a closure!");;


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
        let exp1' = big_step exp1 env in
        let exp2' = big_step exp2 env in
        (match cond' with
        | Vbool(true) -> exp1'
        | Vbool(false) -> exp2'
        | _-> raise(L1Exception "If: invalid t1 type."))
    | Var(var) -> var_value env var
    | App(exp1, exp2) ->
        let fclos = big_step exp1 env in
        let v' = big_step exp2 env in
        let arg = extract_clos_arg fclos in
        let env' = update_env env arg v' in
        let new_clos = change_clos_env fclos env' in
        let f_name = extract_clos_f new_clos in
        let env' = update_env env' f_name new_clos in
        let e = extract_clos_expr new_clos in
        let value = var_value env' "x" in
        print_value value;
        big_step e env'
    | Let(var, var_type, exp1, exp2) ->
        let var_val = big_step exp1 env in
        let env' = update_env env var var_val in
        big_step exp2 env'
    | Lrec(f_name, in_type, out_type, arg, arg_type, exp1, exp2) ->
        let f_clos = Vrclos(f_name, arg, exp1, env) in
        let env' = update_env env f_name f_clos in
        big_step exp2 env'
    | _-> raise(L1Exception "Expression not implemented.");;


let env = [];;

let fat_exp = Lrec("fat", TyInt, TyInt, "x", TyInt,
    If(Bop(Eq, Var("x"), Num(0)),
        Num(1),
        Bop(Mult, Var("x"), App(Var("fat"), Bop(Diff, Var("x"), Num(1))))),
    App(Var("fat"), Num(5)));;

let result = big_step fat_exp env;;
print_value result;;
