#use "l1_def.ml"

exception NoRuleApplies
exception WrongType of string * string * term
exception NoMatch of string * string * term * term
exception UnexpectedError of string 

let rec type_infer (t:term) : term = 
	match t with
    | TmInteger -> TmInteger (* T INT *)
    | TmBool -> TmBool (* T BOOL *)
    | TmPlus(x1,x2) -> (* T PLUS *)
        let TmInteger = type_infer(TmInteger) in
        let TmBool = type_infer(TmBool) in

    	let x1Type = type_infer(x1) in
		let x2Type = type_infer(x2) in
        
        let rule1 = x1Type == x2Type in
        let rule2 = x1Type == TmInteger in
        let rule3 = x2Type == TmInteger in
     
        let rule1Text = "Tipos de X1 e X2 devem ser iguais,recebido: " in
        let rule2Text = "Tipo de X1 deve ser TmInteger, recebido: " in
        let rule3Text = "Tipo de X2 deve ser TmInteger, recebido: " in

		if rule1 && rule2 && rule3 then
		    x1Type		
		else
            if not(rule1) then
              raise (NoMatch("TmPlus", rule1Text, x1Type, x2Type))
            else if not(rule2) then
              raise (WrongType("TmPlus", rule2Text, x1Type))
            else if not(rule3) then
			  raise (WrongType("TmPlus", rule3Text, x1Type))
            else
              raise (UnexpectedError ("at TmPlus"))
		    
    | TmMinus(x1,x2) -> TmPlus(x1,x2)
    | TmMult(x1,x2) -> TmPlus(x1,x2)
    | TmDiv(x1,x2) -> TmPlus(x1,x2)
    | TmLesser(x1,x2) -> TmPlus(x1,x2)
    | TmLesserOrEqual(x1,x2) -> TmPlus(x1,x2)
    | TmEqual(x1,x2) -> TmPlus(x1,x2)
    | TmNotEqual(x1,x2) -> TmPlus(x1,x2)
    | TmGreater(x1,x2) -> TmPlus(x1,x2)
    | TmGreaterOrEqual(x1,x2) -> TmPlus(x1,x2)

    | TmIf(x1,x2,x3) -> (*tIf*)
        let TmInteger = type_infer(TmInteger) in
        let TmBool = type_infer(TmBool) in

	    let x1Type = type_infer(x1) in
		let x2Type = type_infer(x2) in
		let x3Type = type_infer(x3) in
        
		let rule1 = x1Type == TmBool in
        let rule2 = x2Type == x3Type in
        
        let rule1Text = "Tipo de X1 deve ser TmBool, recebido: " in
        let rule2Text = "Tipos de X2 e X3 devem ser iguais,recebido: " in

        if rule1 && rule2 then
            x1Type      
        else
            if not(rule1) then
              raise (WrongType("TmIf", rule1Text, x1Type))
            else if not(rule2) then
              raise (NoMatch("TmPlus", rule2Text, x2Type, x3Type))
            else
              raise (UnexpectedError ("at TmIf"))

    | _ -> raise NoRuleApplies