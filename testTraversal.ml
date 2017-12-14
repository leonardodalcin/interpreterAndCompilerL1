type variable = string


exception Failure of string

let rec list_max xs =
      match xs with
      | [] ->   (* empty list: fail *)
         failwith "list_max called on empty list"
      | [x] -> (* single element list: return the element *)
         x
      | x :: remainder -> (* multiple element list: recursive case *)
         max x (list_max remainder);;

let turnIntoVar x : variable = 
    x
let rec find_env x environment : value =
       match environment with
            | [] -> raise (NotInEnvironment(x,"not found in environment"))
            | hd::tl -> if fst hd == x then snd hd
                                  else find_env x tl
let environment : (variable * value) list = [("x", Vnum(2))]
let x: variable = "x"

let lista = [1,2,3,4,5]