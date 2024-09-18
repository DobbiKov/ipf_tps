
let rec guess n essais = 
    let () = print_endline "Write your number: " in
    let user_num = read_int () in
    let () = print_endline "\n" in

    if n = user_num then
        print_endline "Trouve"
    else
        let () = if user_num > n then
            print_endline "Trop grand!"
        else 
            print_endline "Trop petit!"
        in
        let () = Printf.printf "Vous avez essaye deja %d fois\n" essais in
        guess n (essais + 1)

let rand_gen = Random.self_init ()
let rand_num = Random.int 101
let () = guess rand_num 1