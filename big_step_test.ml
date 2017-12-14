#use "big_step.ml"


let fat_exp = Lrec("fat", TyInt, TyInt, "x", TyInt,
    If(Bop(Eq, Var("x"), Num(0)),
        Num(1),
        Bop(Mult, Var("x"), App(Var("fat"), Bop(Diff, Var("x"), Num(1))))),
    App(Var("fat"), Num(12)));;

let result = eval fat_exp;;
print_value result;;
