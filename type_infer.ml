#use "l1_def.ml"


let rec airthmetic_infer exp1_type exp2_type =
    if exp1_type == TyInt && exp2_type == TyInt then
        TyInt
    else
         raise (L1Exception "Invalid type in arithmetic operator!")


let rec comparator_infer exp1_type exp2_type =
    if exp1_type == TyInt && exp2_type == TyInt then
        TyBool
    else
         raise (L1Exception "Invalid type in comparassion operator!")


let rec type_infer exp env =
    match exp with
        | Num(n) -> TyInt
        | Bool(b) -> TyBool
        | Bop(op, exp1, exp2) ->
            let exp1_type = type_infer exp1 env in
            let exp2_type = type_infer exp2 env in
            (match op with
            | Sum -> airthmetic_infer exp1_type exp2_type
            | Diff -> airthmetic_infer exp1_type exp2_type
            | Mult -> airthmetic_infer exp1_type exp2_type
            | Div -> airthmetic_infer exp1_type exp2_type
            | Ge -> comparator_infer exp1_type exp2_type
            | Geq -> comparator_infer exp1_type exp2_type
            | Eq -> comparator_infer exp1_type exp2_type
            | Neq -> comparator_infer exp1_type exp2_type
            | Leq -> comparator_infer exp1_type exp2_type
            | Le -> comparator_infer exp1_type exp2_type)
        | If(cond, exp1, exp2) ->
            let cond_type = type_infer cond env in
            let exp1_type = type_infer exp1 env in
            let exp2_type = type_infer exp2 env in
            if cond_type != TyBool then
                raise (L1Exception "Invalid type for if conditional!")
            else if exp1_type == exp2_type then
                exp1_type
            else
                raise (L1Exception "Incomatible types in if!")
        | Var(id) ->
            let value = var_value env id in
            (match value with
            | Vnum(v) -> TyInt
            | Vbool(b) -> TyBool
            | Vclos(var, expr, clos_env) ->
                let var_exp = Var(var) in
                let var_type = type_infer var_exp env in
                let expr_type = type_infer expr env in
                TyFn(var_type, expr_type)
            | Vrclos(f, arg, expr, clos_env) ->
                print_value value;
                let arg_expr = Var(arg) in
                let arg_type = type_infer arg_expr clos_env in
                let expr_type = type_infer expr env in
                TyFn(arg_type, expr_type))
        | App(exp1, exp2) ->
            let exp1_type = type_infer exp1 env in
            let exp2_type = type_infer exp2 env in
            (match exp1_type with
            | TyFn(t1, t2) ->
                if t1 == exp2_type then
                    t2
                else
                    raise (L1Exception "Incompatible types in app.!")
            | _ -> raise (L1Exception "First expression in app. is not a function!"))
        | Lam(var, var_type, exp) ->
            let exp_type = type_infer exp env in
            TyFn(var_type, exp_type)
        | Let(var, var_type, exp1, exp2) ->
            let exp1_type = type_infer exp1 env in
            if var_type != exp1_type then
                raise (L1Exception "Incompatible types in let!")
            else
                type_infer exp2 env
        | Lrec(f, t1, t2, arg, arg_type, exp1, exp2) ->
            if t1 != arg_type then
                raise (L1Exception "Incompatible types in let rec!")
            else
                let exp1_type = type_infer exp1 env in
                if exp1_type != t2 then
                    raise (L1Exception "Incompatible types in let rec!")
                else
                    type_infer exp2 env

