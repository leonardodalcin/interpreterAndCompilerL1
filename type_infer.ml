#use "l1_def.ml"

exception NoRuleApplies
exception WrongType of string * string * tipo
exception NoMatch of string * string * tipo * tipo
exception UnexpectedError of string 

let rec type_infer (t) : tipo = 
    match t with
        | Num(n) -> TyInt
        | Bool(b) -> TyBool
        | Var(x) -> variable;
        | Let(x,t,e1,e2) -> 
            let tType = type_infer(t) in
            let e1Type = type_infer(e) in
            let e2Type = type_infer(e) in

            let ruleText = "Tipo de E deveria ser igual à: " in

            if(tType == e1Type) then
                e2Type
            else
                raise (NoMatch("Let", ruleText, tType, e1Type))

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
            match o with
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
                        e1Type      
                    else
                        raise (NoMatch("Eq", rule1Text, e1Type, e2Type))
                | Neq -> type_infer(Bop(Eq, e1, e2))
                | _ -> raise NoRuleApplies
        | _ -> raise NoRuleApplies

let tif = (Bop(Neq, Bool(true), Num(2)));;