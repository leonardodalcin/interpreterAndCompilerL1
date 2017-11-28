#use "type_infer.ml"

let test : term =
	try
		let tif = TmIf(TmBool,TmPlus(TmBool, TmInteger),TmBool) in
		type_infer(tif)
	with
		| NoMatch -> true
		| WrongType -> true
		| NoRuleApplies -> true
		| _ -> UnexpectedError