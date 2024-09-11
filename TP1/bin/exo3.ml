
let rec somme_entiers n =
    if n = 0 then
        0
    else 
        n + somme_entiers (n-1)

let somme = somme_entiers 5
let () = Printf.printf "Somme entiers: %d\n" somme

let rec somme_carres n =
    if n <= 1 then
        0
    else
        ( (n-1)*(n-1) ) + somme_carres (n-1)

let somme_c = somme_carres 5
let () = Printf.printf "Somme carres: %d\n" somme_c


let rec leibniz n = 
    if n < 0 then
        0.
    else 
        ( float_of_int(-1)**float_of_int(n) ) *. (1. /. ((2.*. float_of_int(n)) +. 1.)) +. leibniz(n-1)

let somme_leib = leibniz 50000
let () = Printf.printf "Somme leibniz: %f\n" somme_leib