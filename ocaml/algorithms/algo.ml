module List = Batteries.List 

(* In this code, I will try not to use the battery package *)

let rec reverse_list r = function
  | h::t -> reverse_list r t @ [h]
  | [] -> r

let rec hd = function
  | h::t when t = [] -> t
  | h::t -> hd t
  | [] -> []

let rec pr_list = function
  | h::t when t <> [] -> print_int h ; print_string ";"; pr_list t
  | h::t -> print_int h
  | [] -> ()

let print_list l =
  let () = print_string "[" in
  let () = pr_list l in
  print_string "]"

let rec nth i = function
  | h::t when i = 0 -> h
  | h::t -> nth (i-1) t
  | [] -> failwith ("Invalid location "^(string_of_int i))

let rec fetch_last = function
  | h::t when t = [] -> ([],h)
  | h::t -> 
    let (l,lst) = fetch_last t in
    (h::l,lst)
  | [] -> failwith ("Incorrect alg")


(*  Creating subsets using the graycode approach *)
let rec generate_subsets_graycode l = 
  reverse_list [] l 


let () = 
  let l = [1;2;3;4] in
  let (l,lst) = fetch_last l in 
  let l = generate_subsets_graycode l in
  let () = print_endline ""; print_list l in
  ()
