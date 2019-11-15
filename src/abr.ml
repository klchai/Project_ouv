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
let rec ajoute (arbre : abr) value =
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
    Printf.printf "list : ";
    let rec helper arr = 
        match arr with
        | [] -> Printf.printf "#";
        | x::rest -> 
            Printf.printf "%d " x;
            helper rest;
    in helper l;
    Printf.printf "\n";
;;

(* Question 2.4 *)
let rec abr_mot (a: abr) =
    match a with
    | Empty -> ""
    | Node(v, l, r)-> "(" ^ (abr_mot l) ^ ")" ^ (abr_mot r)
;;

let isomorphe (a: abr) (b: abr) = 
    let am = abr_mot a in
    let bm = abr_mot b in
    Printf.printf "a: %s\n" am;
    Printf.printf "b: %s\n" bm;
    am == bm
;;

type pair = None | Nodep of string * (int list)
let print_pair p = 
    match p with
    | None -> Printf.printf "" ;
    | Nodep(s, v_list)-> Printf.printf "%s :" s; printList v_list;
;;

let rec print_pair_list p_List =
    match p_List with
    |[] -> Printf.printf "" ;
    |a::rest -> print_pair a; 
                print_pair_list rest;
;;

let modify_list list v =
    match !list with
    |[] -> list:=  [v]
    |a::rest -> list:= v::a::rest
;;

let rec ajoute_mot_value mot valeur list =
    match !list with
    |[] -> list := [Nodep(mot, [valeur])]; Printf.printf "(%s: %d)\n" mot valeur ;
    |None::rest -> ajoute_mot_value mot valeur (ref rest)
    |Nodep(s, v_list)::rest ->
        Printf.printf "(%s: %d) Nodep %s" mot valeur s; printList v_list;
        if s == mot then modify_list (ref v_list) valeur
        else ajoute_mot_value mot valeur (ref rest)
;;


let change_Arbre_to_pairList abr =
    let list = [] in
    let rec helper abr list =
    match abr with
    |Empty -> "";
    |Node(v, l, r)->
        let mot = "(" ^ (helper l list) ^ ")" ^ (helper r list) in
        ajoute_mot_value mot v list;
        mot
    in (helper abr (ref list)); 
    list
;;


(* ------- main ------- *)
(* let arr = gen_permutation 10 
let () = printList arr
let b = ajouteList Empty arr *)
let a_list = [4; 2; 1; 3; 8; 6; 5; 7; 9]
let tmp = ref [9; 11]
let a = ajouteList Empty a_list
let p_list = change_Arbre_to_pairList a
let () = print_pair_list p_list