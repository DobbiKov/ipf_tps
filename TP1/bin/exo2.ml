let () = print_endline "Enter the name of the file:"


let _ = 
    if Array.length Sys.argv <= 1 then 
        print_endline "Pas assez d'arguments"
    else 
        let filename = Sys.argv.(1) in
        let _ = Printf.printf "Vous avez ecrit: %s\n" filename in
        let isexists = Sys.file_exists filename in 

        if isexists then 
            if Sys.is_directory filename then
                Printf.printf "The directory %s exists\n" filename
            else
                Printf.printf "The file %s exists\n" filename 
        else 
            Printf.printf "The file %s doesn't exist\n" filename 