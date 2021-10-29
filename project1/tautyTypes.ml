(* PROPOSITION. Represent an expression in propositional logic. *)

type proposition =
  False |
  True |
  Var of string |
  And of proposition * proposition |
  Or of proposition * proposition |
  Not of proposition |
  Imply of proposition * proposition |
  Equiv of proposition * proposition ;;

(* CONDITIONAL. Represent an IF term. *)

type conditional =
  IffyFalse |
  IffyTrue |
  IffyVar of string |
  If of conditional * conditional * conditional ;;

let rec ifify p =
  match p with
    False -> IffyFalse |
    True -> IffyTrue |
    Var x -> IffyVar x |
    Not a -> If (ifify a, IffyFalse, IffyTrue) |
    And (a, b) -> If (ifify a, ifify b, IffyFalse)|
    Or (a, b) -> If (ifify a, IffyTrue, ifify b)|
    Imply (a, b) -> If (ifify a, ifify b, IffyTrue)|
    Equiv (a, b) -> If (ifify a, ifify b, If (ifify b, IffyFalse, IffyTrue));;

let rec normalize c =
  match c
  with If((If (pi, a1, b1)), a2, b2)->
           let z = (normalize pi) in
           let x = If((normalize a1), (normalize a2), (normalize b2))
           and y = If((normalize b1), (normalize a2), (normalize b2)) in
           (match z
             with IffyFalse -> If(z, x, y)|
                IffyTrue -> If(z, x, y) |
                IffyVar _ -> If(z, x, y)|
                _ -> normalize (If(z, x, y))) |
       If(a, b, c) -> If(a, normalize b, normalize c) |
       _ -> c;;

let rec substitute c v b =
  match c
  with IffyVar x ->
       if x = v
       then ifify b
       else c |
       If (z, x, y) ->
       If((substitute z v b), (substitute x v b), (substitute y v b)) |
       _ -> c;;


let rec simplify c=
  match c
  with If(IffyTrue, a, b) -> simplify a |
       If(IffyFalse, a, b) -> simplify b|
       If(pi, a, b) ->
          (match pi
          with IffyVar x ->
               if (simplify (substitute a x True)) = (simplify (substitute b x False))
               then (simplify (substitute a x True))
               else If(pi, (simplify (substitute a x True)), (simplify (substitute b x False))) |
               _ ->
               if (simplify a) = (simplify b)
               then (simplify a)
               else If(pi, (simplify a), (simplify b))) |
        _ -> c;;

let tautology c =
  if simplify(normalize(ifify c)) = IffyTrue
  then True
  else False;;


(* Q1. A test case. This is ¬ (α ∧ β) → (¬ α ∨ ¬ β). It's a tautology. *)

let q1 =
  (Imply
    (Not
      (And
        (Var "p", Var "q")),
     Or
      (Not
        (Var "p"),
       Not
        (Var "q")))) ;;

(* Q2. A test case. This is α ∧ β. It's not a tautology. *)

let q2 =
  (And
    (Var "p", Var "q")) ;;


tautology q1;;
tautology q2;;

(* Beware: Q1 and Q2 are not exhaustive tests! Your code is not necessarily
   correct if it works on them. *)




(*test result
type proposition =
    False
  | True
  | Var of string
  | And of proposition * proposition
  | Or of proposition * proposition
  | Not of proposition
  | Imply of proposition * proposition
  | Equiv of proposition * proposition
type conditional =
    IffyFalse
  | IffyTrue
  | IffyVar of string
  | If of conditional * conditional * conditional
val ifify : proposition -> conditional = <fun>
val normalize : conditional -> conditional = <fun>
val substitute : conditional -> string -> proposition -> conditional = <fun>
val simplify : conditional -> conditional = <fun>
val tautology : proposition -> proposition = <fun>
val q1 : proposition =
  Imply (Not (And (Var "p", Var "q")), Or (Not (Var "p"), Not (Var "q")))
val q2 : proposition = And (Var "p", Var "q")
- : proposition = True
- : proposition = False
*)
