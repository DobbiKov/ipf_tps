
let rec affiche_tabs i = 
  if i > 0 then
    let () = print_string "    " in
    affiche_tabs (i-1)

let rec affiche_tab_aux i a = 
  if i < Array.length a then
    let () = affiche_tabs 1 in
    let () = Printf.printf "%s\n" (a.(i)) in
    affiche_tab_aux (i+1) a
  else 
    ()


let string_array = [| "Hello"; "World"; "OCaml"; "is"; "fun" |]
let affiche_tab a = affiche_tab_aux 0 a 
let () = affiche_tab Sys.argv 


let rec affiche_dir d = 
  if false = Sys.is_directory d then
    ()
  else
    let () = Printf.printf ": %s\n" d in 
    let t = Sys.readdir d in
    let () = affiche_tab t in
    let () = Sys.chdir d in 
    let () = affiche_dir_iter 0 t in
    let () = Sys.chdir ".." in
    ()
and affiche_dir_iter i t =
  if i < Array.length t then
    let () = affiche_dir t.(i) in
    affiche_dir_iter (i+1) t
  else
    ()

let () = affiche_dir "." 

