open Assoc
open Arbre
open Chaines

(* le type trie :
    triplet arbre, 
            fonction de décomposition mot -> liste de caractères,
            fonction de recomposition liste de caractères -> mot *)

type ('a,'b) trie = Trie of ('b arbre) * ('a -> 'b list) * ('b list -> 'a)



(******************************************************************************)
(*   fonction de création d'un nouveau trie                                   *)
(*   signature  : nouveau :                                                   *)
(*          ('a -> 'b list) -> ('b list -> 'a) -> ('a, 'b) trie = <fun>       *)
(*   paramètres : - une fonction de décomposition                             *)
(*                     mot -> liste de caractères                             *)
(*                -  une fonction de recomposition                            *)
(*                     liste de caractères -> mot                             *)
(*   résultat     : un nouveau trie "vide"                                    *)
(******************************************************************************)

let nouveau fd fr = Trie(Noeud(false,[]), fd, fr)

(******************************************************************************)
(*   fonction d'appartenance d'un élément à un trie                           *)
(*   signature  : appartient : 'a -> ('a, 'b) trie -> bool = <fun>           *)
(*   paramètres : - un mot                                                    *)
(*                - un trie                                                   *)
(*   résultat   : le résultat booléen du test                                 *)
(******************************************************************************)

let rec appartient mot trie = 
    match trie with 
    | Trie(Noeud(b, lf), fd, fr) -> 
        match (fd mot) with 
        | [] -> b
        | c::q -> 
            match (recherche c lf) with 
            | None -> false
            | Some sa -> appartient (fr q) (Trie(sa, fd, fr))

(******************************************************************************)
(*   fonction d'ajout d'un élément dans un trie                               *)
(*   signature  : ajout : 'a -> ('a, 'b) trie -> ('a, 'b) trie = <fun>        *)
(*   paramètres : - un mot                                                    *)
(*                - un trie                                                   *)
(*   résultat   : le trie avec le mot ajouté                                  *)
(******************************************************************************)

let ajout mot (Trie(arbre, decompose, recompose)) = 
    Trie (ajout_arbre (decompose mot) arbre,decompose,recompose)

(** Pour les tests *)

let trie_sujet = List.fold_right ajout 
                                 ["bas"; "bât"; "de"; "la"; "lai"; "laid"; "lait"; "lard"; "le"; "les"; "long"]
                                 (nouveau decompose_chaine recompose_chaine);;

(******************************************************************************)
(*   fonction de retrait d'un élément d'un trie                               *)
(*   signature  : trie_retrait : 'a -> ('a, 'b) trie -> ('a, 'b) trie = <fun> *)
(*   paramètres : - un mot                                                    *)
(*                - un trie                                                   *)
(*   résultat   : le trie avec le mot retiré                                  *)
(******************************************************************************)

let retrait mot (Trie(arbre, decompose, recompose)) = 
    Trie (retrait_arbre (decompose mot) arbre,decompose,recompose)

(******************************************************************************)
(*   fonction interne au Module qui génère la liste de tous les mots          *)
(*   d'un trie                                                                *)
(*   signature    : trie_dico : ('a, 'b) trie -> 'a list = <fun>              *)
(*   paramètre(s) : le trie                                                   *)
(*   résultat     : la liste des mots                                         *)
(******************************************************************************)
<<<<<<< HEAD

let trie_dico (Trie(arbre, decompose, recompose)) = failwith "To Do affiche" 
=======
>>>>>>> 3d9fb400e67c5a3ccbfff35afec69e5ec151294f


 let rec trie_dico (Trie(arbre, decompose, recompose)) =  

    let concat acc lmot =  List.map (fun mot -> recompose (decompose acc) (decompose mot)) lmot in
        match arbre with 
            | Noeud(b, lb) -> List.flatten (List.map (fun (l, sa) -> concat l (trie_dico (Trie(sa, decompose, recompose)))) lb) 

(******************************************************************************)
(* procédure d'affichage d'un trie                                            *)
(*   signature  : affiche : ('a -> unit) -> ('a, 'b) trie -> unit = <fun>      *)
(*   paramètres : - une procédure d'affichage d'un mot                        *)
(*                - un trie                                                   *)
(*   résultat   : aucun                                                       *)
(******************************************************************************)

let affiche p trie = failwith "TO DO affiche"

