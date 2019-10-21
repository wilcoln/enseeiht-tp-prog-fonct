open List

module type Regle =
sig
  type tid = int
  type td
  val id : tid
  
  val appliquer : td -> td list
end

module Regle1: Regle with type td = char list = 
struct
  type tid = int
  type td = char list
  let id = 1
  let appliquer a = 
    match List.rev a with 
    | 'O'::_-> [(a@['A'])]
    | _ -> []
end

module Regle2: Regle with type td = char list = 
struct
  type tid = int
  type td = char list
  let id = 2
  let appliquer a = 
    match a with 
    | 'B'::q-> [(a@q)]
    | _ -> []
end

module Regle3: Regle with type td = char list = 
struct
  type tid = int
  type td = char list
  let id = 3
 
  let appliquer a = 
    let rec remplace a prefix_acc result_acc = 
      match a with
      | [] -> result_acc 
      | 'O'::'O'::'O'::q -> remplace ('O'::'O'::q) (prefix_acc@['O']) ((prefix_acc@('A'::q))::result_acc)
      | 'A'::'O'::'A'::q -> remplace ('O'::'A'::q) (prefix_acc@['A']) ((prefix_acc@('A'::q))::result_acc)
      | t::q -> remplace q (prefix_acc@[t]) result_acc
    in 
    remplace a [] []
end


module Regle4: Regle with type td = char list = 
struct
  type tid = int
  type td = char list
  let id = 4
 
  let appliquer a = 
    let rec remplace a prefix_acc result_acc = 
      match a with
      | [] -> result_acc 
      | 'A'::'A'::q -> remplace ('A'::q) (prefix_acc@['A']) ((prefix_acc@q)::result_acc)
      | t::q -> remplace q (prefix_acc@[t]) result_acc
    in 
    remplace a [] []
end

module Exemples = 
struct 
  let exemple1 = ['B'; 'O']
  let exemple2 = ['B'; 'O'; 'A']
  let exemple3 = ['B'; 'O'; 'O'; 'O'; 'O']
  let exemple4 = ['B'; 'O'; 'A'; 'A'; 'O']
end

let%test _ = (Regle1.appliquer Exemples.exemple1  = [['B';'O';'A']])
let%test _ = (Regle2.appliquer Exemples.exemple2  = [['B';'O';'A';'O';'A']])

module type ArbreReecriture =
sig
  (*
  type tid = int
  type td
  type arbre_reecriture = ...

  val creer_noeud : ...

  val racine : ...
  val fils : ..

  val appartient : td -> arbre_reecriture -> bool
  *)
end