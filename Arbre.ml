

type arbre_lex =  noeud list 
and noeud = Lettre of char * bool * arbre_lex
;;

let rec existe mot arbre = 
	if mot==[] then  true else ( 
	match arbre with
	| [] -> false
	| Lettre(c,b,abr)::l -> if (List.hd mot) == c then existe (List.tl mot) abr
							else existe mot l
	)
;;

let rec ajoute mot arbre = 
	if mot ==[] then  arbre else (
 	match arbre with
	| [] -> 
		[Lettre((List.hd mot),false, (ajoute (List.tl mot) []))] 
	| Lettre(c,b,abr)::l -> if abr ==[] && b then raise (Failure "Deja_defini")
					else(

					if (List.hd mot) == c then ajoute (List.tl mot) abr
							else ajoute mot l

						)
	)
;;

let rec construit listmot arbre= 
	match listmot with
	| [] -> arbre
	| a::l -> let abr = ajoute a arbre in construit l abr
;;


let rec affiche arbre = 
	match arbre with
	| [] -> ()
	| Lettre(c,b,abr)::l -> Printf.printf "%c" c ;
							print_newline ();
							affiche abr;
							affiche l
						;;


let mot1 = ['a';'b';'v';'v'] in 
let mot2 = ['b';'a';'a'] in 
let l = [mot1;mot2] in 
let arbre = construit l [] in if (existe mot1 arbre) then print_endline "true";;
(* affiche arbre;; *)




	