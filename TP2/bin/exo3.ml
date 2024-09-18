let rec hanoi dep aux dest n = 
    if n >= 1 then begin
      let () = hanoi dep dest aux (n-1) in
      let () = Printf.printf "Deplacer le disque %d de %s vers %s\n" n dep dest in
      let () = hanoi aux dep dest (n-1) in
      
    ()
    end

let hanoi_aux n = hanoi "A" "B" "C" n
let start_time = Sys.time ()
let () = hanoi_aux 5 
let end_time = Sys.time ()
let exec_time = end_time -. start_time
let () = Printf.printf "Exec time is: %f" exec_time