module List = Batteries.List 

let rec reverse_list r = function
  | h::t -> h :: reverse_list r t
  | [] -> r


(*  Creating subsets using the graycode approach *)
let rec generate_subsets_graycode l = 
  reverse_list [] l 


let () = 
  let l = [1;2;3;4] in
  let l = generate_subsets_graycode l in
  let () = List.iter (fun x -> print_endline (string_of_int x)) l in 
  ()
