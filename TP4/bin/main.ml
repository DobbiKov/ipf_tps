let () = print_endline "Hello, World!"
let hanoi n = 
    let rec hanoi_aux dep mil arr n = 
        if n > 0 then begin
            hanoi_aux dep arr mil (n-1);
            Printf.printf "%s -> %s\n" dep arr;
            hanoi_aux mil dep arr (n-1)
        end
    in
    hanoi_aux "dep" "mil" "arr" n

type piquet = string * int list
type jeu = piquet list

let affiche_piquet p = 
    let rec afficher_list l = 
        match l with 
            [] -> ()
            | el :: ll -> 
                let () = afficher_list ll in
                Printf.printf "%d-" el 
    in 
    let () = Printf.printf "%s|" (fst p) in
    let () = afficher_list (snd p) in
    print_endline ""

let test_piq = ("test", [1;2;3])
let () = affiche_piquet test_piq 

let rec choix_piquet piq nam = 
        match piq with
            [] -> ("", []) 
            | el :: ll ->
                if fst el = nam then
                    el
                else 
                   choix_piquet ll nam 

let piquets = [
    ("dep", [2;3;4]);
    ("mid", []);
    ("arr", [1])
            ]
let m = choix_piquet piquets "arr"
let () = affiche_piquet m 


let affiche_jeu j = 
    let () = Printf.printf "\x1b[2J\x1b[H" in
    let _dep = choix_piquet j "dep" in
    let () = affiche_piquet _dep in
    let _mid = choix_piquet j "mid" in
    let () = affiche_piquet _mid in
    let _arr = choix_piquet j "arr" in
    let () = affiche_piquet _arr in
    Printf.printf "%!"

let () = affiche_jeu piquets
    
let deplace_sommet p1 p2 = 
    match snd p1 with
    [] -> failwith "Le premier piquet est vide"
    | el :: ll -> 
        let () = 
            match snd p2 with
            [] -> ()
            | elp2 :: llp2 -> if el > elp2 then failwith "l'entier du premier disque est plus grand que celui du deuxieme"
        in
        let piq1 = (fst p1, ll) in
        let piq2 = (fst p2, el :: snd p2) in
        let res = (piq1, piq2) in
        res

let joue j src dst autre = 
    let _src = choix_piquet j src in
    let _dst = choix_piquet j dst in
    let _autre = choix_piquet j autre in
    let res1, res2 = deplace_sommet _src _dst in
    [res1;res2; _autre]

let gen_list n = 
    let rec gen_list_aux n_1 res =
        if n_1 = 0 then res
        else gen_list_aux (n_1 - 1) (n_1 :: res)
    in
    gen_list_aux n []
    
let qqch = gen_list 10
let () = print_endline "hrer"
(*
let rec hanoi_aux dep mil arr n = 
        if n > 0 then begin
            hanoi_aux dep arr mil (n-1);
            Printf.printf "%s -> %s\n" dep arr;
            hanoi_aux mil dep arr (n-1)
        end
    in
    hanoi_aux "dep" "mil" "arr" n
*)



let hanoi_list n =
    let rec hanoi_aux piquets dep mil arr n = 
        Unix.sleepf 0.5;
        if n > 0 then begin
            let new_piquets = hanoi_aux piquets dep arr mil (n-1) in
            let new_piquets_2 = joue new_piquets dep arr mil in 
            affiche_jeu piquets;
            let new_piquets_3 = hanoi_aux new_piquets_2 mil dep arr (n-1) in
            new_piquets_3
        end else piquets

    in
    let final = hanoi_aux [
        ("dep", gen_list n);
        ("mid", []);
        ("arr", [])
    ] "dep" "mid" "arr" n in
    affiche_jeu final

let hanoi_list_2 n =
    let rec hanoi_aux piquets dep mil arr n = 
        Unix.sleepf 0.3;
        if n > 0 then begin
            let new_piquets_bis = hanoi_aux piquets dep arr mil (n-1) in
            let new_piquets = hanoi_aux new_piquets_bis mil dep arr (n-1) in
            let new_piquets_2 = joue new_piquets dep mil arr in 
            affiche_jeu new_piquets_2;
            let new_piquets_3 = hanoi_aux new_piquets_2 arr mil dep (n-1) in
            let new_piquets_4 = joue new_piquets_3 mil arr dep in 
            affiche_jeu new_piquets_4;
            let new_piquets_5 = hanoi_aux new_piquets_4 dep arr mil (n-1) in
            let new_piquets_6 = hanoi_aux new_piquets_5 mil dep arr (n-1) in
            new_piquets_6
        end else piquets

    in
    let final = hanoi_aux [
        ("dep", gen_list n);
        ("mid", []);
        ("arr", [])
    ] "dep" "mid" "arr" n in
    affiche_jeu final
    
let () = hanoi_list_2 3

let hanoi_list_3 n =
    let rec hanoi_aux piquets dep mil arr n = 
        Unix.sleepf 0.3;
        if n > 0 then begin
            let j1 = hanoi_aux piquets dep mil arr (n-1) in
            let j2 = hanoi_aux j1 mil dep arr (n-1) in
            (* let new_piquets = hanoi_aux new_piquets_bis mil dep arr (n-1) in *)
            let new_piquets_2 = joue j2 dep mil arr in 
            affiche_jeu new_piquets_2;
            let new_piquets_3 = hanoi_aux new_piquets_2 arr mil dep (n-1) in
            let new_piquets_4 = joue new_piquets_3 mil arr dep in 
            affiche_jeu new_piquets_4;
            let new_piquets_5 = hanoi_aux new_piquets_4 dep arr mil (n-1) in
            let new_piquets_6 = hanoi_aux new_piquets_5 mil dep arr (n-1) in
            new_piquets_6
        end else piquets

    in
    let final = hanoi_aux [
        ("dep", gen_list n);
        ("mid", []);
        ("arr", [])
    ] "dep" "mid" "arr" n in
    affiche_jeu final
    
let () = hanoi_list_2 3
