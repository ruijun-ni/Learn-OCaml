 (* Tautology Checker *)

exception NoSuchKey;;
let alGet pairs key = 
    let rec alGetting pairs =
        match pairs
        with [] -> raise NoSuchKey |
             (otherKey, otherKeyValue) :: otherPairs ->
                if key = otherKey 
                then otherKeyValue
                else alGetting otherPairs
    in alGetting pairs;;

alGet [("a", 2); ("b", 8); ("c", 3)] "a";;


type proposition =
  False |
  True |
  Var of string |
  And of proposition * proposition |
  Or of proposition * proposition |
  Not of proposition |
  Imply of proposition * proposition |
  Equiv of proposition * proposition ;;


let evaluate proposition pairs = 
    let rec evaluating proposition =
        match proposition
        with False -> false |
             True -> true |
             Var name -> alGet pairs name |
             Not right -> not (evaluating right) |
             And (left, right) -> (evaluating left) && (evaluating right) |
             Or (left, right) -> (evaluating left) || (evaluating right) |
             Imply (left, right) -> (not (evaluating left)) || (evaluating right) |
             Equiv (left, right) -> (evaluating left) = (evaluating right) 
    in evaluating proposition;;


evaluate (And (Var "a", Var "b")) [("a", true); ("b", false)];;
evaluate (Equiv (Not (Or (Var "a", Var "b")), And (Not (Var "a"), Not (Var "b")))) [("a", true); ("b", false)];;


let alPut pairs key value =
    (key, value) :: pairs;;

let generateBools etc n =
    let rec generating bools n = 
        match n
        with 0 -> etc bools |
             _ -> generating (false :: bools) (n-1);
                  generating (true :: bools) (n-1)
    in generating [] n;;


let generatePairs etc names =
    let rec generating names pairs =
        match names
        with [] -> etc pairs |
             name :: otherNames ->
                generating otherNames (alPut pairs name false);
                generating otherNames (alPut pairs name true)
    in generating names [];;

let generateAndTestPairs etc names = 
    let rec generating names pairs =
        match names
        with [] -> etc pairs |
             name :: otherNames ->
                generating otherNames (alPut pairs name false) &&
                generating otherNames (alPut pairs name true)
    in generating names [];;

(* let isContradiction prop = 
    not (generateAndTestPairs (fun pairs -> evaluate prop pairs) (names prop));; *)

let isTautology prop = 
    generateAndTestPairs (fun pairs -> evaluate prop pairs) (names prop);;

isTautology And (Not(Var "a"), Var "b");;