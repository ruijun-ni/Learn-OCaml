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


let rec ifify p =
  match p
  with True -> True |
      False -> False |
      Var a -> Var a |
      Not a -> If ((ifify a), False, True) |
      And (a, b) -> If ((ifify a), (ifify b), False) |
      Or (a, b) -> If ((ifify a), True, (ifify b)) |
      Imply (a, b) -> If ((ifify a), (ifify b), True) |
      Equiv (a, b) -> If ((ifify a), (ifify b), (If ((ifify b), False, True))) |
      If (a, b, c) -> If ((ifify a), (ifify b), (ifify c))
;;


let rec normalize c =
  match c 
  with If (If (pi, a1, b1), a2, b2) -> normalize (If ((normalize pi), (If ((normalize a1), a2, b2)), (If ((normalize b1), a2, b2)))) |
       If (pi, a, b) -> If ((normalize pi), (normalize a), (normalize b)) |
       _ -> c
;;


let rec substitute c v b =
  match c 
  with Var a ->
       if a = v 
       then b 
       else c |
       If (pi, left, right) -> If ((substitute pi v b), (substitute left v b), (substitute right v b)) |
       _ -> c ;;
       

let rec simplify c =
  match c
  with If (True, a, b) -> simplify a |
       If (False, a, b) -> simplify b |
       If (pi, a, b) ->
          (match pi
           with Var x ->
                  if (simplify (substitute a x True)) = (simplify (substitute b x False))
                  then (simplify (substitute a x True))
                  else If(pi, (simplify (substitute a x True)), (simplify (substitute b x False))) |
               _ -> if (simplify a) = (simplify b)
                    then (simplify a)
                    else If(pi, (simplify a), (simplify b))) |
        _ -> c;;


let tautology c =
  if simplify(normalize(ifify c)) = True
  then True 
  else False ;;


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

(* TEST 3 *)
tautology (And (Var "p", Var "q")) ;;
(* - : proposition = False *)


(* OUTPUT:

    type proposition =
        False
      | True
      | Var of string
      | And of proposition * proposition
      | Or of proposition * proposition
      | Not of proposition
      | Imply of proposition * proposition
      | Equiv of proposition * proposition
      | If of proposition * proposition * proposition
    val ifify : proposition -> proposition = <fun>
    val normalize : proposition -> proposition = <fun>
    val substitute : proposition -> string -> proposition -> proposition = <fun>
    val simplify : proposition -> proposition = <fun>
    val tautology : proposition -> proposition = <fun>
    - : proposition = True
    val deMorgan : proposition =
      Equiv (And (Var "p", Var "q"), Not (Or (Not (Var "p"), Not (Var "q"))))
    - : proposition = True
    - : proposition = False

 *)
