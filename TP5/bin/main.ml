type movie = { 
    id : int;
    title : string;
    year : int;
    runtime : int;
    rank : int
}

type res = Movie of movie | Invalid | Eof

let input_movie in_c =
  try
    let s = input_line in_c in
    match String.split_on_char ';' s with
    [ s_id; title; s_year; s_runtime ; s_rank ] ->
       Movie ({
        id = int_of_string s_id;
        title = title;
        year = int_of_string s_year;
        runtime = int_of_string s_runtime;
        rank = int_of_string s_rank;
      })
    | _ -> Invalid

  with
   End_of_file -> Eof
  | _ -> Invalid
;;

let load_movies f =
  let in_c = open_in f in
  let rec loop in_c acc =
    match input_movie in_c with
    | Eof -> acc
    | Invalid -> loop in_c acc
    | Movie m -> loop in_c (m :: acc)
  in
    let res = loop in_c [] in
    close_in in_c;
    res
;;

let movies = load_movies "movies.csv"

let pr_movie m = 
    Printf.printf "{ id=%d; title=%s; year=%d; runtime=%d; rank=%d\n" m.id m.title m.year m.runtime m.rank

let pr_movies ms =
    List.iter (fun m -> pr_movie m) ms 

let moviesTop10 ms = 
    List.filter (fun m -> m.rank <= 10) ms

let movies1980 ms = 
    List.filter (fun m -> m.year >= 1980 && m.year <= 1989) ms

let movie_titles ms = 
    List.map(fun m -> m.title) ms

let max_id ms =
    let f a x = if a > x.id then a else x.id in
    List.fold_left f 0 ms

let average_runtime ms = 
    let f a x = ((fst a) + 1, (snd a) + x.runtime) in 
    let start_el = (0, 0) in
    let res = List.fold_left f start_el  ms in
    float_of_int(snd res) /. float_of_int(fst res)

let average_by_year ms = 
    let n_l = List.sort (fun m1 m2 -> m1.year - m2.year) ms in  
    let rec loop lst acc = 
        match lst with
        | [] -> acc
        | m :: n_lst -> 
            match acc with
        [] -> loop n_lst [(m.year, [m])]
        |y_m :: s_arr -> 
            if m.year == (fst y_m) then
                let tmp_acc = (m.year, m :: snd y_m) :: s_arr in
                loop n_lst tmp_acc 
            else 
                let tmp_acc = (m.year, [m]) :: y_m :: s_arr in 
                loop n_lst tmp_acc 
    in
    let res = loop n_l [] in
    List.map (fun el -> (fst el, average_runtime (snd el))) res


let () = List.iter (fun s -> Printf.printf "%s\n" s) (movie_titles movies)
let () = pr_movies (movies1980 movies)
let () = Printf.printf "%d\n" (max_id movies)
let () = Printf.printf "%f\n" (average_runtime movies)

;;
