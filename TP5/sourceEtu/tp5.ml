(* Evaluation des expressions simples *)


(* Module abstrayant les expressions *)
module type ExprSimple =
sig
  type t
  val const : int -> t
  val plus : t -> t -> t
  val mult : t-> t -> t
end

<<<<<<< HEAD
=======
module type ExprVar = 
sig
  include ExprSimple
  val def : string*t -> t -> t
  val eval: string -> t
end

module type Expr = 
sig
  include ExprVar
end

>>>>>>> 3d9fb400e67c5a3ccbfff35afec69e5ec151294f
(* Module réalisant l'évaluation d'une expression *)
module EvalSimple : ExprSimple with type t = int =
struct
  type t = int
  let const c = c
  let plus e1 e2 = e1 + e2
  let mult e1 e2 = e1 * e2
end


(* Solution 1 pour tester *)
(* A l'aide de foncteur *)

(* Définition des expressions *)
module ExemplesSimples (E:ExprSimple) =
struct
  (* 1+(2*3) *)
  let exemple1  = E.(plus (const 1) (mult (const 2) (const 3)) )
  (* (5+2)*(2*3) *)
  let exemple2 =  E.(mult (plus (const 5) (const 2)) (mult (const 2) (const 3)) )
end

(* Module d'évaluation des exemples *)
module EvalExemples =  ExemplesSimples (EvalSimple)

let%test _ = (EvalExemples.exemple1 = 7)
let%test _ = (EvalExemples.exemple2 = 42)

<<<<<<< HEAD
(* Solution 2 pour tester *)
(* A l'aide de module de première classe *)

let e1 (type t) (module E : ExprSimple with type t = t) = E.(plus (const 1) (mult (const 2) (const 3)) )

let e2 (type t) (module E : ExprSimple with type t = t) = E.(mult (plus (const 5) (const 2)) (mult (const 2) (const 3)) )

(* définition d'un type 'a expr_simple pour ne pas réécrire toujours la même signature *)
type 'a expr_simple = (module ExprSimple with type t = 'a) -> 'a

let eval (term : 'a expr_simple) = term (module EvalSimple)

let%test _ = (eval e1 = 7)
let%test _ = (eval e2 = 42)

=======
(* Exo 1. 1. Module PrintSimple *)

module PrintSimple : ExprSimple with type t = string =
struct
    type t = string
    let const c = string_of_int c
    let plus e1 e2 = e1^"+"^e2
    let mult e1 e2 = "("^e1^"*"^e2^")" 
end

(* Module d'affichages des exemples *)
module PrintExemples = ExemplesSimples(PrintSimple)

(* Exo 1. 2. Module CompteSimple *)
module CompteSimple : ExprSimple with type t = int = 
struct
    type t = int
    let const c = 0
    let plus e1 e2 = e1 + e2 + 1
    let mult e1 e2 = e1 + e2 + 1
end

(* Module d'affichages des exemples *)
module CompteExemples = ExemplesSimples(CompteSimple)


module PrintVar : Expr with type t = string =
struct
    include PrintSimple
    let eval e1 = e1
    let def (nomvar, e1) e2 = "let "^nomvar^" = "^e1^" in "^e2 
end

module Print = 
struct
include PrintVar
end

(* Définition des expressions *)
module ExemplesVar (E:Expr) =
struct
  (* let x = 11+(2*3) *)
  let exemple1  = E.(def ("x", (plus (const 1) (const 2)))
                          (mult (eval "x") (const 3)))

end

(* Module d'affichages des exemples *)
module PrintExemplesVar = ExemplesVar(PrintVar)
>>>>>>> 3d9fb400e67c5a3ccbfff35afec69e5ec151294f
