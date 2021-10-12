(* left reduce *)
let rightReduce func things thing = 
    let rec rightReducing things =
        match things
        with [] -> thing |
             firstThing :: otherThings ->
                func firstThing (rightReducing otherThings)
    in rightReducing things
;;

let f a b = a + b;;
rightReduce f [4;2;3] 2;;

(* right reduce *)

(* get the last element in the list *)
exception EmptyList;;
let rec last things = 
    match things
    with [] -> raise EmptyList |
         [thing] -> thing |
         _ :: otherThings -> last otherThings
;;

(* get the list without the last element *)
let rec butlast things = 
    match things 
    with [] -> raise EmptyList |
         [_] -> [] |
         firstThing :: otherThings -> firstThing :: (butlast otherThings)
;;

let leftReduce func things thing = 
    let rec leftReducing things =
        if things = []
        then thing
        else func (leftReducing (butlast things)) (last things)
    in leftReducing things
;;
leftReduce f [4;2;3] 2;;
