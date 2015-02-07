module Array = Batteries.Array

let (|>) x f = f x
let (>>) f g x = f (g x)

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
    let () = print_string "]" in
    flush stdout

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
      match i-1 = ii with
      | true -> [f ii]
      | false -> (f ii)::(init_i i (ii+1) f)
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

  let rec filter f = function 
    | h::t -> 
      (match f h with | true -> h :: (filter f t) | _ -> filter f t)
    | [] -> []

  let rec exists f = function
    | h::t -> 
      (match f h with
       | true -> true
       | false -> exists f t)
    | [] -> false


  let rec get_graycode ?k l = 
    match length l with 
    | 1 -> [[]; [nth 0 l]]
    | _ ->
      let (l,lst) = fetch_last l in
      let l1 = 
        match k with
        | Some k -> get_graycode ~k:k l
        | None -> get_graycode l
      in
      let l2 = reverse l1 in
      let l2 = 
        match k with
        | Some k -> filter (fun x -> if ((length x)+1) <= k then true else false) l2
        | None -> l2
      in
      let l2 = map (fun x -> x @ [lst]) l2 in
      l1 @ l2

  let rec get_k_subsets k l =
    let l = filter (fun x -> if length x = k then true else false ) l in
    l

  let rec findi f l = 
    fr 0 f l
  and fr i f = function
    | h::t -> 
      (match f i h with
       | true -> (i,h)
       | false -> fr (i+1) f t)
    | [] -> raise Not_found

  let rec for_all f = function
    | h::t -> if f h then for_all f t else false
    | [] -> true

  let rank e l = 
    findi (fun i x -> for_all (fun y -> exists (fun x -> x=y) e ) x ) l

end 

module Math =
struct 

  let rec factorial i = 
    match Big_int.int_of_big_int i with
    | 1 -> i 
    | n -> Big_int.mult_big_int i (factorial (Big_int.sub_big_int i (Big_int.big_int_of_int 1)))

  let rec combination n k =
    Big_int.div_big_int (factorial n) (Big_int.mult_big_int (factorial k)  (factorial (Big_int.sub_big_int n  k)))

end



let generate_subsets ?k l =
  match k with
  | Some k -> 
    let l = List.get_graycode ~k:k l in
    List.get_k_subsets k l
  | None -> List.get_graycode l

let () = 
  let l = List.init 15 (fun x -> x) in
  let l = List.map (fun x -> x + 1) l in
  let l1 = generate_subsets ~k:3 l in
  let l2 = generate_subsets ~k:6 l in
(*   let tot = Math.combination (Big_int.big_int_of_int 15) (Big_int.big_int_of_int 6) in *)
  let tot = List.length l1 in
  let bv = Array.of_list @@ List.init tot (fun x -> false) in
(*   let ss = List.map (fun x -> fst @@ List.rank x l1) l3 in *)
  let i = ref 0 in
  while !i <= tot do
    ()
  done
