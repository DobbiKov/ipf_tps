
type frac = {num: int; denom: int}
type my_num = Int of int | Float of float | Frac of frac


let rec pgcd a b = 
    if b = 0 then a
    else pgcd b (a mod b)

let sign i = 
    if i = 0 then 0
    else if i > 0 then 1
    else -1

let simp f = 
    let _pgcd = pgcd f.num f.denom in
    {num = (f.num / _pgcd) * (sign f.denom); denom = (f.denom / _pgcd) * sign f.denom}

let frac a b =
    simp {num = a; denom = b}

let rec add_frac f1 f2 = 
    if f1.denom = f2.denom then simp {num = f1.num + f2.num; denom = f1.denom}
    else add_frac {num = f1.num * f2.denom; denom = f1.denom * f2.denom} {num = f2.num * f1.denom; denom = f2.denom * f1.denom}

let neg_frac f = 
    simp {num = (-1) * f.num; denom = f.denom}

let sub_frac f1 f2 = 
    add_frac f1 (neg_frac f2)

let mul_frac f1 f2 = 
    simp {num = f1.num * f2.num; denom = f1.denom * f2.denom;}

let inv_frac f =
    simp {num = f.denom; denom = f.num;}

let div_frac f1 f2 = 
    mul_frac f1 (inv_frac f2)

let string_of_frac f =
    string_of_int(f.num) ^ "/" ^ string_of_int(f.denom)

let float_of_frac f = 
    (float_of_int f.num) /. (float_of_int f.denom)
    

let string_of_num num = 
    match num with
    Int a -> string_of_int a
    | Float b -> string_of_float b
    | Frac c -> string_of_frac c

let exec_op n1 n2 op_i op_f op_fr = 
    match n1, n2 with
    | Int i1, Int i2 -> Int (op_i i1 i2)
    | Float fl1, Float fl2 -> Float (op_f fl1 fl2)
    | Float fl1, Int i2 -> Float (op_f fl1 (float_of_int i2))
    | Int i2, Float fl1 -> Float (op_f (float_of_int i2) fl1)
    | Frac f1, Frac f2 -> Frac (op_fr f1 f2)
    | Float fl1, Frac f2 -> Float (op_f fl1 (float_of_frac f2))
    | Frac f1, Float fl2 -> Float (op_f (float_of_frac f1) fl2)
    | Frac f1, Int i2 -> Frac (op_fr f1 (frac i2 1))
    | Int i1, Frac f2 -> Frac (op_fr (frac i1 1) f2)

let add_num n1 n2 = exec_op n1 n2 (+) (+.) add_frac
let sub_num n1 n2 = exec_op n1 n2 (-) (-.) sub_frac
let mul_num n1 n2 = exec_op n1 n2 ( * ) ( *.) mul_frac
let div_num n1 n2 = exec_op n1 n2 (/) (/.) div_frac

let rec pow n k =
    if k = 0 then Int 1
    else mul_num n (pow n (k-1))

let test_pow = pow (Frac (frac 1 2)) 3
let () = Printf.printf "%s\n" (string_of_num test_pow)

let n_f = frac 11 3
let s_f = frac 14 5
let () = Printf.printf "My frac: %d/%d\n" n_f.num n_f.denom
let added_f = add_frac n_f s_f
let () = Printf.printf "Added fracs: %d/%d\n" added_f.num added_f.denom

(*let res_pgcd = pgcd 10 20
let () = Printf.printf "Pgcd 20 et 10: %d\n" res_pgcd
let () = Printf.printf "Pgcd a et b %d\n" (pgcd 4 7)
*)
