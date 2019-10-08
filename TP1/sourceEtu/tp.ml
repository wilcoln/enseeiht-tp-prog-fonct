exception IndexOutOfBounds of (string*int)


(*Exercice 2 *)

(** Nom: coef_dir float*float -> float*flaot -> float
    Role: Calcule et retourne le coefficient directeur de la droite passant par deux points données
    Paramètres: a et b de type float*float, représentant deux points
    Précondition: a, b sont dans R^2
    Postcondition: le résultat est dans R
    Résultat: la pente de la droite
    Erreurs possibles: division par zero, dans ce cas lève une exception.
*)

let coef_dir a b = 
    let denominator = ((fst b) -. (fst a)) in 
        if denominator != 0.
            then ((snd b) -. (snd a)) /. denominator
        else raise Division_by_zero

let%test _ = coef_dir (1., 2.) (5., 3.) = 0.25
let%test _ = coef_dir (0., 2.) (4., 0.) = -0.5
let%test _ = coef_dir (1., 2.) (1., 3.) = infinity

(*Exercice 3 *)

(** Nom: compare coords_int int*int -> bool
    Role: Compare les coordonnées du point
    Paramètre: a est de type int*int, représente un vecteur à deux dimensions
    Précondition: a  est dans N^2
    Postcondition: le résultat est booléen
    Resultat: booléen, comparaison des deux compostantes du vecteur a
*)
let compare_coords_int (a : int*int) = (fst a) = (snd a)

let%test _ = compare_coords_int (1, 2) = false
let%test _ = compare_coords_int (1, 1) = true

(** Nom: is_null
    Role: Dit si un entier est nul ou pas
    Paramètre:  a de type int
*)
let is_null (a : int) = a = 0

let%test _ = is_null 1 = false
let%test _ = is_null 0 = true

(** Nom: bridge
    Role: Renvoie la valeur du paramètre reçu
    Paramètre:  a
    Résultat boolean, indiquant si a est nul 
*)
let bridge a = a

let%test _ = bridge 1 = 1
let%test _ = bridge "wil" = "wil"


(** Nom: compare coords a'*a' -> bool
    Role: Compare les composantes d'un couple
    Paramètre: a un couple avec ses composantes de même types: a'*a'
*)
let compare_coords (a: 'a*'a) = (fst a) = (snd a)

let%test _ = compare_coords (1., 1.) = true
let%test _ = compare_coords (1., 4.) = false
let%test _ = compare_coords ("wil", "wil") = true


(** Nom: project_first a'*b' -> a'
    Role: Renvoie la première composante d'un vecteur à deux dimensions
    Paramètre: a, un couple générique : a'*b'
*)
let project_first a = fst a

let%test _ = project_first (1., 2.) = 1.
let%test _ = project_first (1., "wil") = 1.
let%test _ = project_first ("joe", "wil") = "joe"


(*Exercice 4 *)

(** Nom: project a'*a'*a' -> a'
    Paramètre: a,  un triplet homogène, i, un indice entier (int)
    Précondition:  1 <= i  <= 3
    Role: Retourne la ième composante de a
    Resultat: La ième composante de a *)

let project ((x, y, z), i) = 
    if i = 1 then x
    else if i = 2 then y
    else if i = 3 then z
    else raise (IndexOutOfBounds ("l'indice doit être 1 ou 2 ou 3", i))

let%test _ = project ((1,2,3), 2) = 2


(*Exercice 5 *)

(** Nom: pgcd int*int -> int
    Paramètre: a,  un triplet homogène, i, un indice entier (int)
    Précondition:  1 <= i  <= 3
    Role: Retourne la ième composante de a
    Resultat: La ième composante de a *)

let rec pgcd (a, b) = 
    if a = b then a
    else if a > b then pgcd (a-b, b)
    else pgcd (a, b-a)

let%test _ = pgcd (1, 2) = 1
let%test _ = pgcd (6, 2) = 2
let%test _ = pgcd (6, 12) = 6



(*Exercice 6 *)

(** Nom: podavan int -> int
    Paramètre: n, un entier, indice dont on veut la valeur du terme associé
    Précondition:  n >= 0
    Role: calcul et retourne le nème terme de la suite de padovan
    Resultat: nème terme de la suite de padovan *)

    let rec padovan n = 
        if n = 0 || n = 1 || n = 2 then 1
        else (padovan (n-2)) + (padovan (n-3))

    let%test _ = padovan 1 = 1
    let%test _ = padovan 3 = 2
    let%test _ = padovan 5 = 3


    (** Nom: podavan2 int -> int
    Paramètre: n, un entier, indice dont on veut la valeur du terme associé
    Précondition:  n >= 0
    Role: calcul et retourne le nème terme de la suite de padovan
    Resultat: nème terme de la suite de padovan *)

    let padovan2 n =
        if n = 0 || n = 1 || n = 2 then 1
        else  
            let rec padovan_aux_p p padovan_pm1 padovan_pm2 padovan_pm3 = 
                let padovan_p = padovan_pm2 + padovan_pm3
                in 
                    if p = n then padovan_p
                    else padovan_aux_p (p+1) padovan_p padovan_pm1 padovan_pm2 
        in padovan_aux_p 3 1 1 1

        let%test _ = padovan 1 = 1
        let%test _ = padovan 3 = 2
        let%test _ = padovan 5 = 3


(*Exercice 7 *)

(** Nom: est_premier int -> bool
    Paramètre: n, un entier
    Role: détermine si n est premier ou non
    Resultat: un boolean qui indique si le nombre est premier ou non *)

    let rec est_premier n = 
       let rec est_diviseur d n = 
            if d*d > n then false
            else if n mod d = 0 then true
            else est_diviseur (d+1) n 
        in not (est_diviseur 2 n)

    let%test _ = est_premier 2 = true
    let%test _ = est_premier 4 = false
    let%test _ = est_premier 17 = true


(* open Graphics *)
(* open Affichage *)
(** Création de l'écran d'affichage *)
(* let _ = open_graph " 800x600" *)

(** Fermeture de l'écran d'affichage *)
(* close_graph() *)