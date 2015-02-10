module Array = Batteries.Array
module Hashtbl = Batteries.Hashtbl
module DynArray = Batteries.DynArray

let (|>) x f = f x
let (>>) f g x = f (g x)

(*
 * n=15, k=j=5, l=4 mine=152, best=137
 * n=15, k=j=6, l=3 mine=6
 *)
let nnum = 5
let knum = 3
let jnum = 3
let lnum = 2

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

  let rec find_option f = function
    | h::t -> (match f h with | true -> Some h | false -> find_option f t)
    | [] -> None

  let rec for_all f = function
    | h::t -> if f h then for_all f t else false
    | [] -> true

  let rank e l = 
    findi (fun i x -> for_all (fun y -> exists (fun x -> x=y) e ) x ) l

  let rec fold_left f e = function
    | h::t -> fold_left f (f e h) t
    | [] -> e

  let rec intersect l1 l2 = 
    filter (fun x -> exists (fun y -> x=y ) l2 ) l1

  let subseteq s u = 
    for_all (fun x -> exists (fun y -> x=y) u ) s


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

let get_next_ticket l1 l2 bv1 tbl =
  let ticket = DynArray.fold_left (fun a b -> 
      let (quality,rank_map_l1) = match Hashtbl.find_option tbl b with
        | Some rank_map_l1 -> 
          let quality = List.fold_left (fun a (i,_) -> 
              match bv1.(i) with | true -> a | _ -> a + 1 
            ) 0 rank_map_l1 in
          (quality,rank_map_l1)
        | None ->
          let l_subsets = generate_subsets ~k:lnum b in
          let rank_map_l1 = List.map (fun x -> List.rank x l1 ) l_subsets in
          let quality = List.fold_left (fun a (i,_) -> 
              match bv1.(i) with | true -> a | _ -> a + 1 
            ) 0 rank_map_l1 in
          let () = Hashtbl.add tbl b rank_map_l1 in
          (quality,rank_map_l1)
      in
      match a with
      | None -> Some (quality,b,rank_map_l1)
      | Some (q,e,_) ->
        if quality > q then 
          Some (quality,b,rank_map_l1)
        else a
    ) None l2
  in
  match ticket with
  | Some (_,b,r) as a ->
      let idx = DynArray.index_of (fun x -> x=b) l2 in
      let () = DynArray.delete l2 idx in
      let () = List.iter (fun (i,_) -> bv1.(i) <- true ) r in
      a
  | None -> None

let rec optimize tickets lnum =
  let h = List.fold_left (fun a x -> 
      let h = List.find_option (fun y ->
          match x = y with
          | true -> false
          | false ->
            let s = List.intersect x y in
            List.length s >= lnum
        ) tickets
      in
      match a with 
      | None ->
        (match h with
         | None -> a
         | _ as x -> x)
      | _ -> a
    ) None tickets 
  in
  match h with
  | None -> tickets
  | Some x -> 
    let tickets = List.filter (fun y -> y <> x ) tickets in
    optimize tickets lnum

let rec remove_dp l2 = function
  | h::t -> 
    let (i,e) = List.findi (fun i x -> List.subseteq h x ) l2 in
    e :: (remove_dp l2 t)
  | [] -> []


let () = 
  let l = List.init nnum (fun x -> x) in
  let l = List.map (fun x -> x + 1) l in
  let l1 = generate_subsets ~k:lnum l in
  let l2 = generate_subsets ~k:knum l in
  let l3 = generate_subsets ~k:jnum l in
  let l2 = remove_dp l2 l3 in
(*   let tot = Math.combination (Big_int.big_int_of_int 15) (Big_int.big_int_of_int 6) in *)
  let tot = List.length l1 in
  let bv1 = Array.of_list @@ List.init tot (fun x -> false) in
(*   let ss = List.map (fun x -> fst @@ List.rank x l1) l3 in *)
  let num = ref 0 in
  let tbl = Hashtbl.create 5000 in
  let l2 = DynArray.of_list l2 in
  let tickets =
    let tt = ref [] in
    (while Array.exists (fun x -> x=false) bv1 do
       let ticket = get_next_ticket l1 l2 bv1 tbl in
       let lll = Array.to_list bv1 in
       let falses = List.fold_left (fun a x -> if x = false then a+1 else a) 0 lll in
       let trues = List.fold_left (fun a x -> if x = true then a+1 else a) 0 lll in
       let (a,b,c) = match ticket with | Some x -> x | None -> failwith "No more tickets to buy" in
       let () = print_string @@ "Ticket : ";  List.print_list b ; print_endline "" in
       print_endline @@ "trues "^ (string_of_int @@ trues)^" falses "^(string_of_int @@ falses);
       let () = tt := b :: !tt in
       match ticket with
       | None -> failwith "Incorrect algo"
       | _ -> num := !num + 1
     done);
    print_endline @@ "Total number of tickets without opt : "^ string_of_int @@ !num;
    !tt
  in
  let tickets = optimize tickets lnum in
  print_endline @@ "Total number of tickets after opt : "^ string_of_int @@ List.length tickets;
(*   List.iter (fun x -> List.print_list x ) tickets; *)
