type abr = Empty | Node of int * abr * abr

(* Question 1.1 *)
let rec remove_at list n =
    match list with
    | [] -> []
    | head::rest -> 
        if n == 0 then rest
        else head::remove_at rest (n - 1)
;;

let rec makelist n =
    if n == 0 then []
    else n::makelist (n - 1)
;;

let extraction_alea l p =
    match l with
    | [] -> l, p
    | head::rest ->
        let len = List.length l in
        let r = Random.int len in
        (remove_at l r), ((List.nth l r)::p)
;;

(* Question 1.2 *)
let gen_permutation n =
    let l = makelist n in
    let rec helper l p n =
        if n == 0 then p
        else 
            let l, p = extraction_alea l p in
            helper l p (n - 1)
    in helper l [] n;
;;

(* Question 1.3 *)
let rec ajoute arbre value =
    match arbre with
    | Empty -> Node(value, Empty, Empty)
    | Node(v, left, right) ->
    if value < v then Node(v, ajoute left value, right)
    else Node(v, left, ajoute right value)
;;

let rec ajouteList arbre list = 
    match list with
    | [] -> arbre
    | x::rest -> ajouteList (ajoute arbre x) rest
;;

let search arbre key =
    let rec dfs arbre key =
    match arbre with
    | Empty -> false
    | Node(v, left, right) ->
        if v == key then true
        else if key < v then dfs left key
        else dfs right key
    in dfs arbre key;
;;

let printArbre arbre =
    Printf.printf "abr : \n";
    let rec dfs arbre = 
        match arbre with
        | Empty -> Printf.printf "";
        | Node(v, left, right) ->
            dfs left;
            Printf.printf "%d " v; 
            dfs right;
    in dfs arbre;
    Printf.printf "\n";
;;
let printList l =
    Printf.printf "list : \n";
    let rec bfs arr = 
        match arr with
        | [] -> Printf.printf "";
        | x::rest -> 
            Printf.printf "%d " x;
            bfs rest;
    in bfs l;
    Printf.printf "\n";
;;

(* Question 2.4 *)



(* ------- main ------- *)
let arr = gen_permutation 10
let () = printList arr
let res = ajouteList Empty arr 
let () = printArbre res
let () = Printf.printf "%b\n" (search res 5);