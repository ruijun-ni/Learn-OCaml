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


(* ================================= *)


(* let alPut pairs key value =
  (key, value) :: pairs ;;


let generateAndTestPairs etc names =
  let rec generatingAndTestingPairs names pairs =
    match names
    with [] ->
           etc pairs |
         name :: names ->
           generatingAndTestingPairs names (alPut pairs name false) &&
           generatingAndTestingPairs names (alPut pairs name true)
  in generatingAndTestingPairs names [] ;; *)

(* ================================= *)


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
            
let ifExp = ifify (Imply ((Not (And ((Var "p"), (Var "q")))), (Or ((Not (Var "p")), (Not (Var "q"))))));;



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

let normalizedIF = normalize ifExp;;      



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

(* substitute normalizedIF "p" True;; *)


let simplify c =
    let rec normalizing c = 
        match c 
        with True -> True |
             False -> False |
             Var a -> Var a |
             If (True, a, b) -> (normalizing a) |
             If (False, a, b) -> (normalizing b) |
             If (pi, True, False) -> (normalizing pi) |
             If (pi, a, b) -> 
                if a = b
                then (normalizing a)
                else (If ((normalizing pi), (normalizing a), (normalizing b)))
    in normalizing c
;;

let simpliedIF = simplify normalizedIF;;



(* 

If (Var "p",
 If (Var "q", True, If (Var "p", If (Var "q", False, True), True)),
 If (Var "p", If (Var "q", False, True), True))

 *)
 
let try1 = substitute simpliedIF "p" True;;

 (* 
 
 If (True, 
     If (Var "q", True, If (True, If (Var "q", False, True), True)),
     If (True, If (Var "q", False, True), True)
    )

  *)

let try2 = substitute try1 "q" True;;

(* 

If (True, 
    If (True, True, If (True, If (True, False, True), True)),
    If (True, If (True, False, True), True))

 *)