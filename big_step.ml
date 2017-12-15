#use "l1_def.ml"
#use "type_infer.ml"

(* Big step *)


let rec big_step exp env =
    type_infer exp env;
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
    big_step exp env;
