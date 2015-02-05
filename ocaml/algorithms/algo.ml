module List = Batteries.List 

(**
 * Creating subsets using the graycode approach
 *)
let generate_subsets_graycode l = 
  l


let () = 
  let l = [1;2;3;4] in
  let l = generate_subsets_graycode l in
  let () = List.iter (fun x -> print_endline (string_of_int x)) l in 
  ()
