(*
  CSci 2041 Lab Assignment 7
  
  Ruijun Ni
  nee00011

    James Moen
    25 Oct 21

  It's worth 35 points.
*)

(* MUTY QUEUE. A mutable queue of BASEs, as a circular doubly linked list. The
   type doesn't say that the list is circular or doubly linked. That's done by
   the functions that manipulate MUTY QUEUEs. All those functions must work in
   O(1) time. *)

type 'base mutyQueue =
  MutyQueueNode of
   'base *
   'base mutyQueue ref *
   'base mutyQueue ref ;;

(* 

  Put your code for MUTY QUEUE MAKE, MUTY QUEUE EMPTY, MUTY QUEUE ENQUEUE and
  MUTY QUEUE DEQUEUE here.

*)

let mutyQueueMake s = 
  let rec temp = MutyQueueNode (s, ref temp, ref temp)
  in temp;;


let mutyQueueEmpty q = 
  match q 
  with MutyQueueNode (elem, left, right) -> 
      (((! left) == q) && ((! right) == q));;
    

let mutyQueueEnqueue q e =
  match q
  with MutyQueueNode (elem, left, right) -> 
    match !left
    with MutyQueueNode(elem1, left1, right1) ->
      let newElem = MutyQueueNode(e, ref !left, ref q)
      in right1 := newElem;
         left := newElem;;


let mutyQueueDequeue q =
  match q
  with MutyQueueNode(elem, next, prev) ->
    if mutyQueueEmpty q
    then elem
    else match ! prev
         with MutyQueueNode(elem1, next1, prev1) ->
              match ! prev1
              with MutyQueueNode(elem2, next2, prev2) ->
                 prev := !prev1;
                 next2 := q;
                 elem1;;




(* Make a QUEUE whose sentinel is the empty string "" and test it. The comments
   say what each test should return, and how many points you get (if any) for
   successful tests. *)

let queue = mutyQueueMake "" ;;

(* 2 pt. MutyQueueNode ("", {contents = <cycle>}, {contents = <cycle>}) *)

mutyQueueEmpty queue ;;           (* 2 pt. true *)

mutyQueueDequeue queue ;;         (* 2 pt. "" *)

mutyQueueEnqueue queue "A" ;;     (* 1 pt. () *)

mutyQueueEmpty queue ;;           (* 2 pt. false *)

mutyQueueEnqueue queue "B" ;;     (* 1 pt. () *)

mutyQueueEnqueue queue "C" ;;     (* 1 pt. () *)

mutyQueueDequeue queue ;;         (* 5 pt. "A" *)

mutyQueueDequeue queue ;;         (* 5 pt. "B" *)

mutyQueueDequeue queue ;;         (* 5 pt. "C" *)

mutyQueueEmpty queue ;;           (* 2 pt. true *)

mutyQueueDequeue queue ;;         (* 5 pt. "" *)

mutyQueueDequeue queue ;;         (* 2 pt. "" *)
