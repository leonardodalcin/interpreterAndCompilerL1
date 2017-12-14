#use "l1_def.ml"

exception NoRuleApplies
exception WrongType of string * string * tipo
exception NoMatch of string * string * tipo * tipo
exception UnexpectedError of string 

let environment : (variable * value) list = [("x", Vnum(2)); ("fat", Vclos("x", Bop(Sum, 1,2)),environment)];;

let rec type_infer (t) : tipo = 
    match t with
        | Num(n) -> TyInt
        | Bool(b) -> TyBool
        | Var(x) -> 
            let varValue = find_var_value x environment in
            (match varValue with
               | Vnum(n) -> TyInt 
               | Vbool(b) -> TyBool
               | Vclos(x,e,environment) -> 
                    let ruleText = "Tipo de E deveria ser igual à: " in
                    let varType = type_infer(Var(x)) in
                    let eType = type_infer(e) in
                    if(eType == varType) then
                        eType
                    else
                        raise (NoMatch("Vclos", ruleText, varType, eType))

               | Vrclos(x1,x2, e, environment) -> 
                    let ruleText = "Tipo de E deveria ser igual à: " in
                    let x1Type = type_infer(Var(x1)) in
                    let x2Type = type_infer(Var(x2)) in
                    let eType = type_infer(e) in
                    if (eType == x1Type) && (eType==x2Type) then
                        eType
                    else
                        raise (NoMatch("Vrclos", ruleText, x1Type, eType))
               | _ -> raise NoRuleApplies)

        | Let(x,t,e1,e2) -> 
            let xType = type_infer(Var(x)) in
            let tType = t in
            let e1Type = type_infer(e1) in
            let e2Type = type_infer(e2) in

            let ruleText = "Tipo de E deveria ser igual à: " in

            if(tType == e1Type) then
                e2Type
            else
                raise (NoMatch("Let", ruleText, tType, e1Type))
     (*    | Lrec(x1, t1, t2, x2, t, e1, e2) ->
            let x1Type = type_infer(Var(x1)) in
            let x2Type = type_infer(Var(x2)) in *)
        | App(e1,e2) -> 
            let e1Type = type_infer(e1) in
            let e2Type = type_infer(e2) in

            let ruleText = "Tipo de E2 deveria ser igual à: " in

            if e1Type == TyFn(TyBool, TyBool) then
                if(e2Type == TyBool) then
                    TyBool
                else 
                    raise (NoMatch("App", ruleText, e2Type, TyBool))
            else if e1Type == TyFn(TyBool, TyInt) then
                if(e2Type == TyBool) then
                    TyInt
                else 
                    raise (NoMatch("App", ruleText, e2Type, TyBool))
            else if e1Type == TyFn(TyInt, TyInt) then
                if(e2Type == TyInt) then
                    TyInt
                else 
                    raise (NoMatch("App", ruleText, e2Type, TyInt))
            else if e1Type == TyFn(TyInt, TyBool) then
                if(e2Type == TyInt) then
                    TyBool
                else 
                    raise (NoMatch("App", ruleText, e2Type, TyInt))

            else raise (UnexpectedError("At App"))

        | If(e1,e2,e3) -> (*TIf*)
            let rule1Text = "Tipo de E1 deve ser TmBool, recebido: " in
            let rule2Text = "Tipos de E2 e E3 devem ser iguais,recebido: " in

            let e1Type = type_infer(e1) in
            let e2Type = type_infer(e2) in
            let e3Type = type_infer(e3) in
            
            let rule1 = e1Type == TyBool in
            let rule2 = e2Type == e3Type in
            
            if rule1 && rule2 then
                e2Type      
            else
                if not(rule1) then
                  raise (WrongType("TIf", rule1Text, e1Type))
                else if not(rule2) then
                  raise (NoMatch("TIf", rule2Text, e2Type, e3Type))
                else
                  raise (UnexpectedError ("at If"))

        | Bop(o,e1,e2) -> 
            let e1Type = type_infer(e1) in
            let e2Type = type_infer(e2) in
            (match o with
                | Sum ->  
                    let rule1Text = "Tipo de E1 deve ser TyInt, recebido: " in
                    let rule2Text = "Tipo de E2 deve ser TyInt, recebido: " in

                    let rule1 = e1Type == TyInt in
                    let rule2 = e2Type == TyInt in

                    if rule1 && rule2 then
                        TyInt
                    else 
                        if not(rule1) then
                          raise (WrongType("Sum", rule1Text, e1Type))
                        else if not(rule2) then
                          raise (WrongType("Sum", rule2Text, e2Type))
                        else
                          raise (UnexpectedError ("at Sum"))

                | Diff -> type_infer(Bop(Sum, e1, e2))
                | Mult -> type_infer(Bop(Sum, e1, e2))
                | Div -> type_infer(Bop(Sum, e1, e2))
                | Ge -> 
                    let rule1Text = "Tipo de E1 deve ser TyInt, recebido: " in
                    let rule2Text = "Tipo de E2 deve ser TyInt, recebido: " in

                    let rule1 = e1Type == TyInt in
                    let rule2 = e2Type == TyInt in

                    if rule1 && rule2 then
                        TyBool
                    else 
                        if not(rule1) then
                          raise (WrongType("Ge", rule1Text, e1Type))
                        else if not(rule2) then
                          raise (WrongType("Ge", rule2Text, e2Type))
                        else
                          raise (UnexpectedError ("at Ge"))

                | Geq -> type_infer(Bop(Ge, e1, e2))
                | Leq -> type_infer(Bop(Ge, e1, e2))
                | Le -> type_infer(Bop(Ge, e1, e2))
                | Eq -> 
                    let rule1Text = "Tipos de E1 e E2 devem ser iguais,recebido: " in
                    let rule1 = e1Type == e2Type in
                    
                    if rule1 then
                        TyBool      
                    else
                        raise (NoMatch("Eq", rule1Text, e1Type, e2Type))
                | Neq -> type_infer(Bop(Eq, e1, e2))
                | _ -> raise NoRuleApplies)
        | _ -> raise NoRuleApplies

let tif = (If(Bop(Eq, Var("x"), Num(0)),Num(1)), Bop(Mult, Var("x"), App(Var("fat"), Bop(Diff, Var("x"), Num(1)))),App(Var("fat"), Num(5)));;

