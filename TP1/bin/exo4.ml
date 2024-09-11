let bissextile a = 
    if a mod 400 = 0 then
        true
    else if a mod 4 = 0 && a mod 100 != 0 then
        true
    else 
        false

let year = 2300
let is_biss = bissextile year
let () = if is_biss then
    Printf.printf "The %d is bissextile\n" year
else
    Printf.printf "The %d is not bissextile\n" year
    
let jour_mois m a = 
    if m = 2 then
        let is_bisse = bissextile a in
        if is_bisse then
            29
        else 
            28
    else if m < 8 then
        if m mod 2 = 0 then
            30
        else 
            31
    else 
        let new_m = m - 1 in
        if new_m mod 2 = 0 then
            30
        else 
            31
