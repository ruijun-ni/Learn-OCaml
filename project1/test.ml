type proposition = 
  False | 
  True | 
  Var of string | 
  And of proposition * proposition | 
  Or of proposition * proposition | 
  Not of proposition | 
  Imply of proposition * proposition | 
  Equiv of proposition * proposition | 
  If of proposition * proposition * proposition ;;


let ifify p =
    let rec ififying p =
        match p
        with True -> True |
             False -> False |
             Var a -> Var a |
             Not a -> If ((ififying a), False, True) |
             And (a, b) -> If ((ififying a), (ififying b), False) |
             Or (a, b) -> If ((ififying a), True, (ififying b)) |
             Imply (a, b) -> If ((ififying a), (ififying b), True) |
             Equiv (a, b) -> If ((ififying a), (ififying b), (If ((ififying b), False, True))) |
             If (a, b, c) -> If ((ififying a), (ififying b), (ififying c))
    in ififying p
;;

(*           
  let ifExp = ifify (Imply ((Not (And ((Var "p"), (Var "q")))), (Or ((Not (Var "p")), (Not (Var "q"))))));;

  val ifExp : proposition =
    If (If (If (Var "p", Var "q", False), False, True),
    If (If (Var "p", False, True), True, If (Var "q", False, True)), True)
*)


let normalize c =
    let rec normalizing c = 
        match c 
        with True -> True |
             False -> False |
             Var a -> Var a |
             If (If (pi, a1, b1), a2, b2) -> normalizing (If ((normalizing pi), (If ((normalizing a1), a2, b2)), (If ((normalizing b1), a2, b2)))) |
             If (pi, a, b) -> If ((normalizing pi), (normalizing a), (normalizing b))
             (* If (pi, a, b) -> If ((normalizing pi), a, b) *)
    in normalizing c
;;

(*
  let normalizedIF = normalize ifExp;;     

  val normalizedIF : proposition =
    If (Var "p",
    If (Var "q",
      If (False,
      If (Var "p", If (False, True, If (Var "q", False, True)),
        If (True, True, If (Var "q", False, True))),
      True),
      If (True,
      If (Var "p", If (False, True, If (Var "q", False, True)),
        If (True, True, If (Var "q", False, True))),
      True)),
    If (False,
      If (False,
      If (Var "p", If (False, True, If (Var "q", False, True)),
        If (True, True, If (Var "q", False, True))),
      True),
      If (True,
      If (Var "p", If (False, True, If (Var "q", False, True)),
        If (True, True, If (Var "q", False, True))),
      True))) 
*)



let substitute c v b =
    let rec substituting c v b =
        match c 
        with True -> True |
             False -> False |
             Var a -> 
                if a = v 
                then b 
                else Var a |
            If (pi, left, right) -> If ((substituting pi v b), (substituting left v b), (substituting right v b)) 
    in substituting c v b
;;

(*
  example:
  substitute normalizedIF "p" True;; 
*)


let simplify c =
    let rec simplifying c = 
        match c 
        with True -> True |
             False -> False |
             Var a -> Var a |
             If (True, a, b) -> (simplifying a) |
             If (False, a, b) -> (simplifying b) |
             If (pi, True, False) -> pi |
             If (pi, a, b) -> 
                if a = b
                then (simplifying a)
                else (If (pi, (simplifying a), (simplifying b)))
    in simplifying c
;;

(* 
  let simpliedIF = simplify normalizedIF;;

  val simpliedIF : proposition =
    If (Var "p",
    If (Var "q", True, If (Var "p", If (Var "q", False, True), True)),
    If (Var "p", If (Var "q", False, True), True)) 
*)



(* change True and False from "proposition" type to "Boolean" type true and false *)
(* here p is the simplified form *)
let evaluate p =
  match p
  with True -> true |
       False -> false;;

(* a helper function evaluating all four cases *)
(* here p is the simplified form *)
let tautologyHelper p =
  evaluate (simplify (substitute (substitute p "p" True) "q" True)) &&
  evaluate (simplify (substitute (substitute p "p" True) "q" False)) &&
  evaluate (simplify (substitute (substitute p "p" False) "q" True)) &&
  evaluate (simplify (substitute (substitute p "p" False) "q" False)) ;;


let tautology p =
  tautologyHelper (simplify (normalize (ifify p)));;


(* TEST 1*)
tautology (Imply ((Not (And ((Var "p"), (Var "q")))), (Or ((Not (Var "p")), (Not (Var "q"))))));;
(* - : bool = true *)

(* TEST 2 *)
let deMorgan =
  Equiv (
   And (Var "p", Var "q"),
   Not (Or (Not (Var "p"), Not (Var "q")))) ;;
tautology deMorgan;;
(* - : bool = true *)
