
(* In this code, I will try not to use the battery package *)
module List = 
struct
  let rec reverse = function
    | h::t -> reverse t @ [h]
    | [] -> []

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

  let rec init i f = 
    let rec init_i i ii f =
      match i = ii with
      | true -> [f i]
      | false -> (f i)::(init_i i (ii+1) f)
    in
    init_i i 0 f

  let rec length = function
    | h::t -> 1 + (length t)
    | [] -> 0

  let rec last = function
    | h::t ->  (match t with | [] -> h | _ -> last t)
    | [] -> [] 

  let rec map f = function
    | h::t -> (f h) :: (map f t)
    | [] -> []

  let rec iter f = function
    | h::t -> f h; iter f t
    | [] -> ()

  let rec get_graycode l = 
    match length l with 
    | 1 -> [[]; [nth 0 l]]
    | _ ->
      let (l,lst) = fetch_last l in
      let l1 = get_graycode l in
      let l2 = reverse l1 in
      let l2 = map (fun x -> x @ [lst]) l2 in
      l1 @ l2
end 


(*  Creating subsets using the graycode approach *)
let rec generate_subsets l = 
  List.get_graycode l


let () = 
  let l = [1;2;3] in
  let l = generate_subsets l in
  let () = List.iter (fun x -> List.print_list x) l in
  ()
