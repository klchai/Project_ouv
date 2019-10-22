(* type 'a abr = Empty | Node of 'a *'a abr * 'a abr;; *)

(* type 'a abr_le = noeud list;; *)
type node = Letter of char * bool * abr_lex
and abr_lex = node list

let rec exist mot tree =
    let aux word index len =
        match tree with
        | [] -> false
        | Letter(c, b, nodes)::rest_tree -> 
            if c == word.[index] then 
                if len == 1 then b
                else exist (String.sub word (index + 1) (len - 1)) rest_tree
            else exist word rest_tree
    in aux mot 0 (String.length mot);
;;

let ajoute mot tree =
    let rec aux mot index len = 
        match tree with
        | [] -> 
            if mot == "" then []
            else 
                if index == len then Letter(mot.[index], true, aux (String.sub (word (index + 1) (len - 1)) index []))
                else Letter(mot.[index], false, aux (String.sub word (index + 1) (len - 1)) index []
        | Letter(c, b, nodes) :: rest_tree ->
            if mot.[index] == c then ajoute (String.sub word (index + 1) (len - 1)) rest_tree
            else Letter(word.[index], false, ajoute (String.sub word (index + 1) (len - 1)) [])::tree
    in aux mot 0 (String.length mot);

let rec print_arbre tree =
    match tree with
    | [] -> 0
    | Letter(c, b, nodes)::rest_tree ->
        print_char c;
        (print_arbre nodes)  + (print_arbre rest_tree)
in

let tree = [] in
let tree = ajoute "ok" tree in
print_arbre tree