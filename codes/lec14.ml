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
