let () = print_endline "Hello, World!"
let f x = x / 17
let res_div = f 42
let () = Printf.printf "Res de divis: %d \n" res_div

let x = 3.14159
let y = 
    let x2 = x *. x in
    x2 *. x2

let () = Printf.printf "Res: %f \n" y