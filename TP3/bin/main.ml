let () = print_endline "Hello, World!"

type color = Red | Green | Blue [@@ocaml.warning "-37"]
type county = { c_name: string; population: int; color: color} [@@ocaml.warning "-69"]
let c = Blue 
let my_c = {c_name = "Ukraine"; population = 41000000; color = Blue}

let color_to_string col = 
    match col with 
    Red -> "Red"
    | Green -> "Green"
    | Blue -> "Blue"

let my_c_str = color_to_string c
let () = Printf.printf "Your color is %s\n" my_c_str
let () = Printf.printf "My country is %s and has color: %s\n" my_c.c_name (color_to_string my_c.color)
